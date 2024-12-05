defmodule Anoma.Node.Transaction.Backends do
  @moduledoc """
  I am the Transaction Backend module.

  I define a set of backends for the execution of the given transaction candidate.
  Currently, I support transparent resource machine (RM) execution as well as
  the following debug executions: read-only, key-value store, and blob store executions.

  ### Public API

  I have the following public functionality:
  - `execute/3`
  """

  alias Anoma.Node
  alias Node.Logging
  alias Node.Transaction.{Executor, Ordering, Storage}
  alias Anoma.TransparentResource
  alias Anoma.TransparentResource.Transaction, as: TTransaction
  alias Anoma.TransparentResource.Resource, as: TResource

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

  @type transaction() :: {backend(), Noun.t() | binary()}

  typedstruct enforce: true, module: ResultEvent do
    @typedoc """
    I hold the content of the Result Event, which conveys the result of
    the transaction candidate code execution on the Anoma VM to
    the Mempool engine.

    ### Fields
    - `:tx_id`              - The transaction id.
    - `:tx_result`          - VM execution result; either :error or an
                              {:ok, noun} tuple.
    """
    field(:tx_id, binary())
    field(:vm_result, {:ok, Noun.t()} | :error)
  end

  typedstruct enforce: true, module: CompleteEvent do
    @typedoc """
    I hold the content of the Complete Event, which communicates the result
    of the transaction candidate execution to the Executor engine.

    ### Fields
    - `:tx_id`              - The transaction id.
    - `:tx_result`          - Execution result; either :error or an
                              {:ok, value} tuple.
    """
    field(:tx_id, binary())
    field(:tx_result, {:ok, any()} | :error)
  end

  typedstruct enforce: true, module: TRMEvent do
    @typedoc """
    I hold the content of the Nullifier Event, which communicates a set of
    nullifiers defined by the actions of the transaction candidate to the
    Intent Pool.

    ### Fields

    - `:commitments`        - The set of commitments.
    - `:nullifiers`         - The set of nullifiers.
    """
    field(:commitments, MapSet.t(binary()))
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

  @doc """
  I execute the specified transaction candidate using the designated backend.
  If the transaction is provided as a `jam`med noun atom, I first attempt
  to apply `cue/1` in order to unpack the transaction code.

  First, I execute the transaction code on the Anoma VM. Next, I apply processing
  logic to the resulting value, dependent on the selected backend.
  - For read-only backend, the value is transmitted as a Result Event.
  - For the key-value and blob store executions, the obtained value is stored
  and a Complete Event is issued.
  - For the transparent Resource Machine (RM) execution, I verify the
    transaction's validity and compute the corresponding set of nullifiers,
    which is transmitted as a Nullifier Event.
  """

  @spec execute(node_id, {back, Noun.t()}, id) :: :ok
        when id: binary(),
             node_id: String.t(),
             back: backend()
  def execute(node_id, {backend, tx_code}, id) do
    env = %Nock{scry_function: fn a -> Ordering.read(node_id, a) end}
    vm_result = vm_execute(tx_code, env, id)
    result_event(id, vm_result, node_id, backend)

    res =
      with {:ok, vm_res} <- vm_result,
           {:ok, backend_res} <- backend_logic(backend, node_id, id, vm_res) do
        {:ok, backend_res}
      else
        _e ->
          empty_write(backend, node_id, id)

          :error
      end

    complete_event(id, res, node_id, backend)
  end

  ############################################################
  #                       VM Execution                       #
  ############################################################

  @spec vm_execute(Noun.t(), Nock.t(), binary()) ::
          {:ok, Noun.t()} | :vm_error
  defp vm_execute(tx_code, env, id) do
    with {:ok, code} <- cue_when_atom(tx_code),
         {:ok, [_ | stage_2_tx]} <- nock(code, [9, 2, 0 | 1], env),
         {:ok, ordered_tx} <- nock(stage_2_tx, [10, [6, 1 | id], 0 | 1], env),
         {:ok, result} <- nock(ordered_tx, [9, 2, 0 | 1], env) do
      {:ok, result}
    else
      _e -> :vm_error
    end
  end

  @spec cue_when_atom(Noun.t()) :: :error | {:ok, Noun.t()}
  defp cue_when_atom(tx_code) when Noun.is_noun_atom(tx_code) do
    Noun.Jam.cue(tx_code)
  end

  defp cue_when_atom(tx_code) do
    {:ok, tx_code}
  end

  ############################################################
  #                     Backend Execution                    #
  ############################################################

  @spec backend_logic(backend(), String.t(), binary(), Noun.t()) ::
          :error | {:ok, any()}
  defp backend_logic(:debug_term_storage, node_id, id, vm_res) do
    store_value(node_id, id, vm_res)
  end

  defp backend_logic({:debug_read_term, pid}, node_id, id, vm_res) do
    send_value(node_id, id, vm_res, pid)
  end

  defp backend_logic(:debug_bloblike, node_id, id, vm_res) do
    blob_store(node_id, id, vm_res)
  end

  defp backend_logic(:transparent_resource, node_id, id, vm_res) do
    transparent_resource_tx(node_id, id, vm_res)
  end

  @spec transparent_resource_tx(String.t(), binary(), Noun.t()) ::
          {:ok, any} | :error
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

      Ordering.add(
        node_id,
        {id,
         %{
           append: [
             {anoma_keyspace("nullifiers"), map.nullifiers},
             {anoma_keyspace("commitments"), map.commitments}
           ],
           write: [{anoma_keyspace("anchor"), value(map.commitments)}]
         }}
      )

      transparent_rm_event(map.commitments, map.nullifiers, node_id)

      {:ok, tx}
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
    stored_commitments =
      Ordering.read(node_id, {id, anoma_keyspace("commitments")})

    stored_nullifiers =
      Ordering.read(node_id, {id, anoma_keyspace("nullifiers")})

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
      root_coms =
        Storage.read(
          node_id,
          {latest_root_time, anoma_keyspace("commitments")}
        )

      for <<"NF_", rest::binary>> <- action_nullifiers,
          reduce: MapSet.new([]) do
        cm_set ->
          if ephemeral?(rest) do
            cm_set
          else
            MapSet.put(cm_set, "CM_" <> rest)
          end
      end
      |> MapSet.subset?(root_coms)
    else
      Enum.all?(action_nullifiers, fn <<"NF_", rest::binary>> ->
        ephemeral?(rest)
      end)
    end
  end

  @spec ephemeral?(Noun.noun_atom()) :: boolean()
  defp ephemeral?(jammed_transaction) do
    nock_boolean =
      Noun.Jam.cue(jammed_transaction)
      |> elem(1)
      |> Noun.list_nock_to_erlang_safe()
      |> elem(1)
      |> List.pop_at(2)
      |> elem(0)

    nock_boolean in [0, <<>>, <<0>>, []]
  end

  @spec send_value(String.t(), binary(), Noun.t(), pid()) ::
          {:ok, any()} | :error
  defp send_value(node_id, id, result, reply_to) do
    # send the value to reply-to address and the topic
    reply_msg = {:read_value, result}
    send(reply_to, reply_msg)
    Ordering.write(node_id, {id, []})
    {:ok, reply_msg}
  end

  @spec blob_store(String.t(), binary(), Noun.t()) :: {:ok, any} | :error
  def blob_store(node_id, id, result) do
    key = :crypto.hash(:sha256, :erlang.term_to_binary(result))
    Ordering.write(node_id, {id, [{key, result}]})
    {:ok, key}
  end

  @spec store_value(String.t(), binary(), Noun.t()) :: {:ok, any} | :error
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

      {:ok, list}
    else
      _ -> :error
    end
  end

  @spec empty_write(backend(), String.t(), binary()) :: :ok
  defp empty_write(_backend, node_id, id) do
    Ordering.write(node_id, {id, []})
  end

  ############################################################
  #                        Helpers                           #
  ############################################################

  @spec complete_event(
          String.t(),
          :error | {:ok, any()},
          String.t(),
          backend()
        ) :: :ok
  defp complete_event(id, result, node_id, backend) do
    event =
      Node.Event.new_with_body(node_id, %__MODULE__.CompleteEvent{
        tx_id: id,
        tx_result: result
      })

    event(backend, event)
  end

  @spec result_event(String.t(), any(), String.t(), backend()) :: :ok
  defp result_event(id, result, node_id, backend) do
    event =
      Node.Event.new_with_body(node_id, %__MODULE__.ResultEvent{
        tx_id: id,
        vm_result: result
      })

    event(backend, event)
  end

  @spec transparent_rm_event(
          MapSet.t(binary()),
          MapSet.t(binary()),
          String.t()
        ) :: :ok
  defp transparent_rm_event(cms, nlfs, node_id) do
    event =
      Node.Event.new_with_body(node_id, %__MODULE__.TRMEvent{
        commitments: cms,
        nullifiers: nlfs
      })

    EventBroker.event(event)
  end

  @doc """
  I am the commitment accumulator add function for the transparent resource
  machine.

  Given the commitment set, I add a commitment to it.
  """

  @spec add(MapSet.t(), binary()) :: MapSet.t()
  def add(acc, cm) do
    MapSet.put(acc, cm)
  end

  @doc """
  I am the commitment accumulator witness function for the transparent
  resource machine.

  Given the commitment set and a commitment, I return the original set if
  the commitment is a member of the former. Otherwise, I return nil
  """

  @spec witness(MapSet.t(), binary()) :: MapSet.t() | nil
  def witness(acc, cm) do
    if MapSet.member?(acc, cm) do
      acc
    end
  end

  @doc """
  I am the commitment accumulator value function for the transparent
  resource machine.

  Given the commitment set, I turn it to binary and then hash it using
  sha-256.
  """

  @spec value(MapSet.t()) :: binary()
  def value(acc) do
    :crypto.hash(:sha256, :erlang.term_to_binary(acc))
  end

  @doc """
  I am the commitment accumulator verify function for the transparent
  resource machine.

  Given the commitment, a witness (i.e. a set) and a commitment value, I
  output true iff the witness's value is the same as the provided value and
  the commitment is indeed in the set.
  """

  @spec verify(binary(), MapSet.t(), binary()) :: bool()
  def verify(cm, w, val) do
    val == value(w) and MapSet.member?(w, cm)
  end

  @spec anoma_keyspace(String.t()) :: list(String.t())
  defp anoma_keyspace(key) do
    ["anoma", key]
  end

  @spec event(backend(), EventBroker.Event.t()) :: :ok
  defp event(_backend, event) do
    EventBroker.event(event)
  end
end
