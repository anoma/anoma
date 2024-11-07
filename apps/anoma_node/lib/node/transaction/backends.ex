defmodule Anoma.Node.Transaction.Backends do
  @moduledoc """
  Backend module.
  Support :kv, :ro, :rm, :cairo execution.
  """

  alias Anoma.Node
  alias Node.Logging
  alias Node.Transaction.{Executor, Ordering, Storage}
  alias Anoma.TransparentResource
  alias Anoma.TransparentResource.Transaction, as: TTransaction
  alias Anoma.TransparentResource.Resource, as: TResource
  alias CommitmentTree.Spec
  alias Anoma.CairoResource.Transaction, as: CTransaction

  import Nock
  require Noun
  require Node.Event
  use EventBroker.DefFilter
  use TypedStruct

  @type backend() ::
          :debug_term_storage
          | {:debug_read_term, pid}
          | :debug_bloblike
          | :transparent_resource
          | :cairo_resource

  @type transaction() :: {backend(), Noun.t() | binary()}

  typedstruct enforce: true, module: ResultEvent do
    field(:tx_id, integer())
    field(:vm_result, {:ok, Noun.t()} | :error)
  end

  typedstruct enforce: true, module: CompleteEvent do
    field(:tx_id, integer())
    field(:tx_result, {:ok, any()} | :error)
  end

  typedstruct enforce: true, module: NullifierEvent do
    field(:nullifiers, MapSet.t(binary()))
  end

  deffilter CompleteFilter do
    %EventBroker.Event{body: %Node.Event{body: %CompleteEvent{}}} ->
      true

    _ ->
      false
  end

  deffilter ForMempoolFilter do
    %EventBroker.Event{body: %Node.Event{body: %ResultEvent{}}} ->
      true

    %EventBroker.Event{body: %Node.Event{body: %Executor.ExecutionEvent{}}} ->
      true

    _ ->
      false
  end

  @spec execute(String.t(), {backend(), Noun.t()}, binary()) :: :ok
  def execute(node_id, {backend, tx}, id)
      when Noun.is_noun_atom(tx) do
    case Nock.Cue.cue(tx) do
      {:ok, tx} ->
        execute(node_id, {backend, tx}, id)

      :error ->
        error_handle(node_id, id)
    end
  end

  def execute(node_id, {{:debug_read_term, pid}, tx}, id) do
    execute_candidate(node_id, tx, id, fn x, y ->
      send_value(node_id, x, y, pid)
    end)
  end

  def execute(node_id, {:debug_term_storage, tx}, id) do
    execute_candidate(node_id, tx, id, &store_value(node_id, &1, &2))
  end

  def execute(node_id, {:debug_bloblike, tx}, id) do
    execute_candidate(node_id, tx, id, &blob_store(node_id, &1, &2))
  end

  def execute(node_id, {:transparent_resource, tx}, id) do
    execute_candidate(
      node_id,
      tx,
      id,
      &transparent_resource_tx(node_id, &1, &2)
    )
  end

  def execute(node_id, {:cairo_resource, tx}, id) do
    execute_candidate(
      node_id,
      tx,
      id,
      &cairo_resource_tx(node_id, &1, &2)
    )
  end

  @spec gate_call(Noun.t(), Nock.t(), binary(), String.t()) ::
          {:ok, Noun.t()} | :vm_error
  defp gate_call(tx_code, env, id, node_id) do
    with {:ok, stage_2_tx} <- nock(tx_code, [9, 2, 0 | 1], env),
         {:ok, ordered_tx} <- nock(stage_2_tx, [10, [6, 1 | id], 0 | 1], env),
         {:ok, result} <- nock(ordered_tx, [9, 2, 0 | 1], env) do
      res = {:ok, result}
      result_event(id, res, node_id)
      res
    else
      _e -> :vm_error
    end
  end

  @spec execute_candidate(node_id, Noun.t(), id, process) :: :ok
        when id: binary(),
             node_id: String.t(),
             process: (id, Noun.t() -> :ok | :error)
  defp execute_candidate(node_id, tx_code, id, process) do
    env = %Nock{scry_function: fn a -> Ordering.read(node_id, a) end}

    with {:ok, result} <- gate_call(tx_code, env, id, node_id),
         :ok <- process.(id, result) do
      :ok
    else
      :vm_error ->
        error_handle(node_id, id)

      _e ->
        Ordering.write(node_id, {id, []})
        complete_event(id, :error, node_id)
    end
  end

  @spec transparent_resource_tx(String.t(), binary(), Noun.t()) ::
          :ok | :error
  defp transparent_resource_tx(node_id, id, result) do
    storage_checks = fn tx -> storage_check?(node_id, id, tx) end
    verify_tx_root = fn tx -> verify_tx_root(node_id, tx) end

    verify_options = [
      double_insertion_closure: storage_checks,
      root_closure: verify_tx_root
    ]

    with {:ok, tx} <- TransparentResource.Transaction.from_noun(result),
         true <- TransparentResource.Transaction.verify(tx, verify_options) do
      map =
        for action <- tx.actions,
            reduce: %{commitments: MapSet.new(), nullifiers: MapSet.new()} do
          %{commitments: cms, nullifiers: nlfs} ->
            %{
              commitments: MapSet.union(cms, action.commitments),
              nullifiers: MapSet.union(nlfs, action.nullifiers)
            }
        end

      ct =
        case Ordering.read(node_id, {id, :ct}) do
          :absent -> CommitmentTree.new(Spec.cm_tree_spec(), nil)
          val -> val
        end

      {ct_new, anchor} =
        CommitmentTree.add(ct, map.commitments |> MapSet.to_list())

      Ordering.add(
        node_id,
        {id,
         %{
           append: [
             {:nullifiers, map.nullifiers},
             {:commitments, map.commitments}
           ],
           write: [{:anchor, anchor}, {:ct, ct_new}]
         }}
      )

      nullifier_event(map.nullifiers, node_id)

      complete_event(id, {:ok, tx}, node_id)

      :ok
    else
      e ->
        unless e == :error do
          Logging.log_event(
            node_id,
            :error,
            "Transaction verification failed. Reason: #{inspect(e)}"
          )
        end

        :error
    end
  end

  @spec verify_tx_root(String.t(), TTransaction.t()) ::
          true | {:error, String.t()}
  defp verify_tx_root(node_id, trans = %TTransaction{}) do
    # TODO improve the error messages
    commitments_exist_in_roots(node_id, trans) or
      {:error, "Nullified resources are not committed at latest root"}
  end

  @spec storage_check?(String.t(), binary(), TTransaction.t()) ::
          true | {:error, String.t()}
  defp storage_check?(node_id, id, trans) do
    stored_commitments = Ordering.read(node_id, {id, :commitments})
    stored_nullifiers = Ordering.read(node_id, {id, :nullifiers})

    # TODO improve error messages
    cond do
      any_nullifiers_already_exist?(stored_nullifiers, trans) ->
        {:error, "A submitted nullifier already exists in storage"}

      any_commitments_already_exist?(stored_commitments, trans) ->
        {:error, "A submitted commitment already exists in storage"}

      true ->
        true
    end
  end

  @spec any_nullifiers_already_exist?(
          {:ok, MapSet.t(TResource.nullifier())} | :absent,
          TTransaction.t()
        ) :: boolean()
  defp any_nullifiers_already_exist?(:absent, _) do
    false
  end

  defp any_nullifiers_already_exist?(
         {:ok, stored_nulls},
         trans = %TTransaction{}
       ) do
    nullifiers = TTransaction.nullifiers(trans)
    Enum.any?(nullifiers, &MapSet.member?(stored_nulls, &1))
  end

  @spec any_commitments_already_exist?(
          {:ok, MapSet.t(TResource.commitment())} | :absent,
          TTransaction.t()
        ) :: boolean()
  defp any_commitments_already_exist?(:absent, _) do
    false
  end

  defp any_commitments_already_exist?(
         {:ok, stored_comms},
         trans = %TTransaction{}
       ) do
    commitments = TTransaction.commitments(trans)
    Enum.any?(commitments, &MapSet.member?(stored_comms, &1))
  end

  @spec commitments_exist_in_roots(String.t(), TTransaction.t()) :: bool()
  defp commitments_exist_in_roots(
         node_id,
         trans = %TTransaction{}
       ) do
    latest_root_time =
      for root <- trans.roots, reduce: 0 do
        time ->
          with {:atomic, [{_, height_list, ^root}]} <-
                 :mnesia.transaction(fn ->
                   :mnesia.match_object(
                     {Storage.updates_table(node_id), root, :_}
                   )
                 end) do
            height = hd(height_list)

            if height > time do
              height
            else
              time
            end
          else
            {:atomic, []} -> time
          end
      end

    action_nullifiers = TTransaction.nullifiers(trans)

    if latest_root_time > 0 do
      root_coms = Storage.read(node_id, {latest_root_time, :commitments})

      for <<"NF_", rest::binary>> <- action_nullifiers,
          reduce: MapSet.new([]) do
        cm_set ->
          if is_ephemeral?(rest) do
            cm_set
          else
            MapSet.put(cm_set, "CM_" <> rest)
          end
      end
      |> MapSet.subset?(root_coms)
    else
      Enum.all?(action_nullifiers, fn <<"NF_", rest::binary>> ->
        is_ephemeral?(rest)
      end)
    end
  end

  defp is_ephemeral?(jammed_transaction) do
    nock_boolean =
      Nock.Cue.cue(jammed_transaction)
      |> elem(1)
      |> List.pop_at(2)
      |> elem(0)

    nock_boolean in [0, <<>>, <<0>>, []]
  end

  @spec send_value(String.t(), binary(), Noun.t(), pid()) :: :ok | :error
  defp send_value(node_id, id, result, reply_to) do
    # send the value to reply-to address and the topic
    reply_msg = {:read_value, result}
    send(reply_to, reply_msg)
    Ordering.write(node_id, {id, []})
    complete_event(id, {:ok, reply_msg}, node_id)
  end

  @spec blob_store(String.t(), binary(), Noun.t()) :: :ok | :error
  def blob_store(node_id, id, result) do
    key = :crypto.hash(:sha256, :erlang.term_to_binary(result))
    Ordering.write(node_id, {id, [{key, result}]})
    complete_event(id, {:ok, key}, node_id)
  end

  @spec store_value(String.t(), binary(), Noun.t()) :: :ok | :error
  def store_value(node_id, id, result) do
    with {:ok, list} <- result |> Noun.list_nock_to_erlang_safe(),
         true <-
           Enum.all?(list, fn
             [_ | _] -> true
             _ -> false
           end) do
      Ordering.write(
        node_id,
        {id, list |> Enum.map(fn [k | v] -> {k, v} end)}
      )

      complete_event(id, {:ok, list}, node_id)
    else
      _ -> :error
    end
  end

  @spec error_handle(String.t(), binary()) :: :ok
  defp error_handle(node_id, id) do
    result_event(id, :error, node_id)
    Ordering.write(node_id, {id, []})
    complete_event(id, :error, node_id)
  end

  @spec complete_event(String.t(), :error | {:ok, any()}, String.t()) :: :ok
  defp complete_event(id, result, node_id) do
    event =
      Node.Event.new_with_body(node_id, %__MODULE__.CompleteEvent{
        tx_id: id,
        tx_result: result
      })

    EventBroker.event(event)
  end

  @spec result_event(String.t(), any(), String.t()) :: :ok
  defp result_event(id, result, node_id) do
    event =
      Node.Event.new_with_body(node_id, %__MODULE__.ResultEvent{
        tx_id: id,
        vm_result: result
      })

    EventBroker.event(event)
  end

  @spec nullifier_event(MapSet.t(binary()), String.t()) :: :ok
  defp nullifier_event(set, node_id) do
    event =
      Node.Event.new_with_body(node_id, %__MODULE__.NullifierEvent{
        nullifiers: set
      })

    EventBroker.event(event)
  end

  @spec cairo_resource_tx(String.t(), binary(), Noun.t()) ::
          :ok | :error
  defp cairo_resource_tx(node_id, id, result) do
    with {:ok, tx} <- CTransaction.from_noun(result),
         true <- Anoma.RM.Transaction.verify(tx),
         true <- root_existence_check(tx, node_id, id),
         # No need to check the commitment existence
         true <- nullifier_existence_check(tx, node_id, id) do
      ct =
        case Ordering.read(node_id, {id, :ct}) do
          :absent -> CTransaction.cm_tree()
          val -> val
        end

      {ct_new, anchor} =
        CommitmentTree.add(ct, tx.commitments)

      Ordering.add(
        node_id,
        {id,
         %{
           append: [
             {:nullifiers, MapSet.new(tx.nullifiers)},
             {:commitments, MapSet.new(tx.commitments)},
             {:roots, MapSet.new([anchor])}
           ],
           write: [{:anchor, anchor}, {:ct, ct_new}]
         }}
      )

      nullifier_event(tx.nullifiers, node_id)

      complete_event(id, {:ok, tx}, node_id)

      :ok
    else
      e ->
        unless e == :error do
          Logging.log_event(
            node_id,
            :error,
            "Transaction verification failed. Reason: #{inspect(e)}"
          )
        end

        :error
    end
  end

  @spec nullifier_existence_check(CTransaction.t(), String.t(), binary()) ::
          true | {:error, String.t()}
  def nullifier_existence_check(transaction, node_id, id) do
    with {:ok, stored_nullifiers} <-
           Ordering.read(node_id, {id, :nullifiers}) do
      if Enum.any?(
           transaction.nullifiers,
           &MapSet.member?(stored_nullifiers, &1)
         ) do
        {:error, "A submitted nullifier already exists in storage"}
      else
        true
      end
    else
      _ -> true
    end
  end

  @spec root_existence_check(STransaction.t(), String.t(), binary()) ::
          true | {:error, String.t()}
  def root_existence_check(transaction, node_id, id) do
    with {:ok, stored_roots} <-
           Ordering.read(node_id, {id, :roots}),
         true <-
           Enum.all?(transaction.roots, &MapSet.member?(stored_roots, &1)) do
      true
    else
      _ -> {:error, "A submitted root dose not exist in storage"}
    end
  end
end
