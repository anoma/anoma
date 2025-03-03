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

  alias Anoma.CairoResource.Transaction, as: CTransaction
  alias Anoma.Node
  alias Anoma.Node.Logging
  alias Anoma.Node.Transaction.Ordering
  alias Anoma.Node.Transaction.Storage
  alias Anoma.RM.Transparent.ComplianceUnit, as: TCU
  alias Anoma.RM.Transparent.Transaction, as: TTransaction
  alias Anoma.RM.Transparent.Primitive.CommitmentAccumulator, as: TAcc
  alias Anoma.Node.Events

  require Node.Event
  require Noun

  import Nock

  use EventBroker.DefFilter
  use TypedStruct

  @type backend() ::
          :debug_term_storage
          | {:read_only, pid}
          | :debug_bloblike
          | :transparent_resource
          | :cairo_resource

  @type transaction() :: {backend(), Noun.t() | binary()}

  deffilter CompleteFilter do
    %EventBroker.Event{body: %Node.Event{body: %Events.CompleteEvent{}}} ->
      true

    _ ->
      false
  end

  deffilter ForMempoolFilter do
    %EventBroker.Event{body: %Node.Event{body: %Events.ResultEvent{}}} ->
      true

    _ ->
      false
  end

  deffilter ForMempoolExecutionFilter do
    %EventBroker.Event{body: %Node.Event{body: %Events.ExecutionEvent{}}} ->
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
  - For read-only backend, the value is sent directly to specified recepient.
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
    time = Storage.current_time(node_id)

    scry =
      fn list ->
        if list do
          with [id, key] <- list |> Noun.list_nock_to_erlang(),
               {:ok, value} <-
                 (case backend do
                    {:read_only, _pid} ->
                      Storage.read(
                        node_id,
                        {time, key |> Noun.list_nock_to_erlang()}
                      )

                    _ ->
                      Ordering.read(
                        node_id,
                        {id, key |> Noun.list_nock_to_erlang()}
                      )
                  end) do
            {:ok, value |> Noun.Nounable.to_noun()}
          else
            _ -> :error
          end
        else
          :error
        end
      end

    env = %Nock{scry_function: scry}
    vm_result = vm_execute(tx_code, env, id)

    event(backend, fn ->
      Events.transaction_result(id, vm_result, node_id, __MODULE__)
    end)

    res =
      with {:ok, vm_res} <- vm_result,
           {:ok, backend_res} <-
             backend_logic(backend, node_id, id, vm_res, time: time) do
        {:ok, backend_res}
      else
        _e ->
          empty_write(backend, node_id, id)

          :error
      end

    event(backend, fn ->
      Events.transaction_complete(id, res, node_id, __MODULE__)
    end)
  end

  ############################################################
  #                       VM Execution                       #
  ############################################################

  @spec event(backend(), (-> :ok)) :: :ok
  defp event({:read_only, _}, _event) do
    :ok
  end

  defp event(_backend, event) do
    event.()
  end

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

  @spec backend_logic(backend(), String.t(), binary(), Noun.t(), list()) ::
          :error | {:ok, any()}
  defp backend_logic(:debug_term_storage, node_id, id, vm_res, _opts) do
    store_value(node_id, id, vm_res)
  end

  defp backend_logic({:read_only, pid}, _node_id, _id, vm_res, opts) do
    send_value(vm_res, pid, opts)
  end

  defp backend_logic(:debug_bloblike, node_id, id, vm_res, _opts) do
    blob_store(node_id, id, vm_res)
  end

  defp backend_logic(:transparent_resource, node_id, id, vm_res, _opts) do
    transparent_resource_tx(node_id, id, vm_res)
  end

  defp backend_logic(:cairo_resource, node_id, id, vm_res, _opts) do
    cairo_resource_tx(node_id, id, vm_res)
  end

  @spec transparent_resource_tx(String.t(), binary(), Noun.t()) ::
          {:ok, any} | :error
  defp transparent_resource_tx(node_id, id, result) do
    with {:ok, tx} <- TTransaction.from_noun(result),
         true <- TTransaction.verify(tx),
         # possibly also add check for CU roots
         true <- storage_check(node_id, id, tx),
         true <- verify_tx_root(node_id, tx) do
      map =
        for action <- tx.actions,
            reduce: %{
              commitments: MapSet.new(),
              nullifiers: MapSet.new(),
              blobs: []
            } do
          %{commitments: cms, nullifiers: nlfs, blobs: blobs} ->
            %{
              commitments: MapSet.union(cms, MapSet.new(action.created)),
              nullifiers: MapSet.union(nlfs, MapSet.new(action.consumed)),
              blobs:
                for {_tag, list} <- action.app_data, reduce: blobs do
                  acc ->
                    for {binary, bool} <- list, reduce: [] do
                      local_acc ->
                        if bool do
                          [
                            {["anoma", "blob", :crypto.hash(:sha256, binary)],
                             binary}
                            | local_acc
                          ]
                        else
                          local_acc
                        end
                    end ++ acc
                end
            }
        end

      old_cms =
        case Ordering.read(node_id, {id, anoma_keyspace("commitments")}) do
          :absent -> MapSet.new()
          {:ok, res} -> res
        end

      writes = [
        {anoma_keyspace("anchor"),
         TAcc.value(
           MapSet.union(
             map.commitments,
             old_cms
           )
         )}
        | map.blobs
      ]

      Ordering.add(
        node_id,
        {id,
         %{
           append: [
             {anoma_keyspace("nullifiers"), map.nullifiers},
             {anoma_keyspace("commitments"), map.commitments}
           ],
           write: writes
         }}
      )

      Events.trm_event(map.commitments, map.nullifiers, node_id, __MODULE__)

      {:ok, tx}
    else
      {:error, msg} ->
        Logging.log_event(
          node_id,
          :error,
          "Transaction verification failed. Reason: #{inspect(msg)}"
        )

      _ ->
        :error
    end
  end

  @spec verify_tx_root(String.t(), TTransaction.t()) ::
          true | {:error, String.t()}
  defp verify_tx_root(node_id, trans = %TTransaction{}) do
    with true <- roots_exist?(node_id, trans) do
      true
    else
      {:error, msg} ->
        {:error, "Root does not exist: " <> msg}
    end
  end

  @spec storage_check(String.t(), binary(), TTransaction.t()) ::
          true | {:error, String.t()}
  defp storage_check(node_id, id, trans) do
    stored_commitments =
      case Ordering.read(node_id, {id, anoma_keyspace("commitments")}) do
        :absent -> MapSet.new()
        {:ok, res} -> res
      end

    stored_nullifiers =
      case Ordering.read(node_id, {id, anoma_keyspace("nullifiers")}) do
        :absent -> MapSet.new()
        {:ok, res} -> res
      end

    {:ok, precis} = TTransaction.action_precis(trans)

    with true <-
           any_nullifiers_already_exist?(stored_nullifiers, precis.consumed),
         true <-
           any_commitments_already_exist?(stored_commitments, precis.created) do
      true
    else
      {:error, msg} -> {:error, msg}
    end
  end

  @spec any_nullifiers_already_exist?(
          MapSet.t(integer),
          MapSet.t(integer)
        ) :: true | {:error, String.t()}
  defp any_nullifiers_already_exist?(
         old_nulfs,
         new_nulfs
       ) do
    case MapSet.intersection(old_nulfs, new_nulfs) |> Enum.to_list() do
      [] -> true
      lst -> {:error, "Nullifiers #{inspect(lst)} already exist"}
    end
  end

  @spec any_commitments_already_exist?(
          MapSet.t(integer),
          MapSet.t(integer())
        ) :: true | {:error, String.t()}
  defp any_commitments_already_exist?(
         old_cms,
         new_cms
       ) do
    case MapSet.intersection(old_cms, new_cms) |> Enum.to_list() do
      [] -> true
      lst -> {:error, "Commitments #{inspect(lst)} already exist"}
    end
  end

  @spec roots_exist?(String.t(), TTransaction.t()) ::
          true | {:error, String.t()}
  defp roots_exist?(
         node_id,
         trans = %TTransaction{}
       ) do
    roots =
      for action <- trans.actions, reduce: MapSet.new() do
        acc ->
          for compliance_unit <- action.compliance_units,
              reduce: MapSet.new() do
            l_acc -> TCU.roots(compliance_unit) |> MapSet.union(l_acc)
          end
          |> MapSet.union(acc)
      end

    Enum.reduce_while(roots, true, fn root, acc ->
      with {:atomic, [{_, {_, _}, ^root}]} <-
             :mnesia.transaction(fn ->
               :mnesia.match_object(
                 {Storage.values_table(node_id),
                  {:_, anoma_keyspace("anchor")}, root}
               )
             end) do
        {:cont, acc}
      else
        {:atomic, []} -> {:halt, {:error, "Root #{inspect(root)} is absent"}}
      end
    end)
  end

  @spec send_value(Noun.t(), pid(), list()) ::
          {:ok, any()}
  defp send_value(result, reply_to, opts) do
    send(reply_to, {opts[:time], result})
    {:ok, result}
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
  defp empty_write({:read_only, _}, _node_id, _id) do
    :ok
  end

  defp empty_write(_backend, node_id, id) do
    Ordering.write(node_id, {id, []})
  end

  @spec cairo_resource_tx(String.t(), binary(), Noun.t()) ::
          :ok | :error
  defp cairo_resource_tx(node_id, id, result) do
    with {:ok, tx} <- CTransaction.from_noun(result),
         true <- CTransaction.verify(tx),
         true <- root_existence_check(tx, node_id, id),
         # No need to check the commitment existence
         true <- nullifier_existence_check(tx, node_id, id) do
      {ct, append_roots} =
        case Ordering.read(node_id, {id, anoma_keyspace("cairo_ct")}) do
          :absent ->
            {CTransaction.cm_tree(),
             MapSet.new([Anoma.Constants.default_cairo_rm_root()])}

          {:ok, val} ->
            {val, MapSet.new()}
        end

      commitments = tx |> CTransaction.commitments()
      nullifiers = tx |> CTransaction.nullifiers() |> MapSet.new()

      {ct_new, anchor} =
        CommitmentTree.add(ct, commitments)

      ciphertexts = tx |> CTransaction.get_cipher_texts() |> MapSet.new()

      write_app_data =
        tx.actions
        |> Enum.flat_map(fn action ->
          action.app_data
          |> Enum.flat_map(fn {_key, value_list} ->
            value_list
            |> Enum.filter(fn {_, deletion} ->
              Noun.equal?(deletion, <<1::256>>)
            end)
            |> Enum.map(fn {value, _} ->
              {["anoma", "blob", "cairo", :crypto.hash(:sha256, value)],
               value}
            end)
          end)
        end)

      Ordering.add(
        node_id,
        {id,
         %{
           append: [
             {anoma_keyspace("cairo_nullifiers"), nullifiers},
             {anoma_keyspace("cairo_roots"),
              MapSet.put(append_roots, anchor)},
             {anoma_keyspace("cairo_ciphertexts"), ciphertexts}
           ],
           write: [{anoma_keyspace("cairo_ct"), ct_new} | write_app_data]
         }}
      )

      Events.srm_event(
        MapSet.new(tx.commitments),
        MapSet.new(tx.nullifiers),
        node_id,
        __MODULE__
      )

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

  @spec nullifier_existence_check(CTransaction.t(), String.t(), binary()) ::
          true | {:error, String.t()}
  def nullifier_existence_check(transaction, node_id, id) do
    with {:ok, stored_nullifiers} <-
           Ordering.read(node_id, {id, anoma_keyspace("cairo_nullifiers")}) do
      if Enum.any?(
           CTransaction.nullifiers(transaction),
           &MapSet.member?(stored_nullifiers, &1)
         ) do
        {:error, "A submitted nullifier already exists in storage"}
      else
        true
      end
    else
      # stored_nullifiers is empty
      _ -> true
    end
  end

  @spec root_existence_check(CTransaction.t(), String.t(), binary()) ::
          true | {:error, String.t()}
  def root_existence_check(transaction, node_id, id) do
    stored_roots =
      case Ordering.read(node_id, {id, anoma_keyspace("cairo_roots")}) do
        :absent -> MapSet.new([Anoma.Constants.default_cairo_rm_root()])
        {:ok, val} -> val
      end

    Enum.all?(transaction.roots, &MapSet.member?(stored_roots, &1)) or
      {:error, "A submitted root dose not exist in storage"}
  end

  ############################################################
  #                        Helpers                           #
  ############################################################

  @spec anoma_keyspace(String.t()) :: list(String.t())
  defp anoma_keyspace(key) do
    ["anoma", key]
  end
end
