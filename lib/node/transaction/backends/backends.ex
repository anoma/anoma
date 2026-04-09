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
  alias Anoma.Node.Transaction.Backends.Events
  alias Anoma.Node.Transaction.Ordering
  alias Anoma.RM.Transparent.ComplianceUnit, as: TCU
  alias Anoma.RM.Transparent.Transaction, as: TTransaction
  alias Anoma.RM.Transparent.Primitive.CommitmentAccumulator, as: TAcc

  require Node.Event
  require Noun

  import Nock

  use TypedStruct

  @type backend() ::
          :debug_term_storage
          | :read_only
          | :transparent_resource
          | :cairo_resource

  @type transaction() :: {backend(), Noun.t() | binary()}

  @doc """
  I execute the specified transaction candidate using the designated backend.
  If the transaction is provided as a `jam`med noun atom, I first attempt
  to apply `cue/1` in order to unpack the transaction code.

  First, I execute the transaction code on the Anoma VM. Next, I apply processing
  logic to the resulting value, dependent on the selected backend.
  - For read-only backend, the value is broadcasted via the ResultEvent.
  - For the key-value and blob store executions, the obtained value is stored
  and a Complete Event is issued.
  - For the transparent Resource Machine (RM) execution, I verify the
    transaction's validity and compute the corresponding set of nullifiers,
    which is transmitted as a Nullifier Event.
  """

  @spec execute(node_id, {back, Noun.t()}, id, reads) :: :ok
        when id: binary(),
             node_id: String.t(),
             back: backend(),
             reads: list(list(binary()))
  def execute(node_id, {backend, tx_code}, id, reads) do
    scry =
      fn list ->
        if list do
          with [id, key] <- list |> Noun.list_nock_to_erlang(),
               true <- Enum.any?(reads, fn x -> Noun.equal?(key, x) end),
               {:ok, value} <-
                 Ordering.read(
                   node_id,
                   {id, key |> Noun.list_nock_to_erlang()}
                 ) do
            {:ok, value |> Noun.Nounable.to_noun()}
          else
            _ -> :error
          end
        else
          :error
        end
      end

    env = %Nock{scry_function: scry}
    vm_result = vm_execute(tx_code, env)
    result_event(id, vm_result, node_id, backend)

    res =
      with {:ok, vm_res} <- vm_result,
           {:ok, backend_res} <-
             backend_logic(backend, node_id, id, vm_res) do
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

  @spec vm_execute(Noun.t(), Nock.t()) ::
          {:ok, Noun.t()} | :vm_error
  defp vm_execute(tx_code, env) do
    with {:ok, result} <- nock(tx_code, [9, 2, 0 | 1], env) do
      {:ok, result}
    else
      _e -> :vm_error
    end
  end

  ############################################################
  #                     Backend Execution                    #
  ############################################################

  @spec backend_logic(backend(), String.t(), binary(), Noun.t()) ::
          :error | {:ok, any()}
  defp backend_logic(:debug_term_storage, node_id, id, vm_res) do
    store_value(node_id, id, vm_res)
  end

  defp backend_logic(:read_only, node_id, id, vm_res) do
    emit_value(node_id, id, vm_res)
  end

  defp backend_logic(:transparent_resource, node_id, id, vm_res) do
    transparent_resource_tx(node_id, id, vm_res)
  end

  defp backend_logic(:cairo_resource, node_id, id, vm_res) do
    cairo_resource_tx(node_id, id, vm_res)
  end

  @spec transparent_resource_tx(String.t(), binary(), Noun.t()) ::
          {:ok, any} | :error
  defp transparent_resource_tx(node_id, id, result) do
    with {:ok, tx} <- TTransaction.from_noun(result),
         true <- TTransaction.verify(tx),
         cms <-
           read_with_default(
             node_id,
             id,
             transparent_keyspace("commitments"),
             MapSet.new()
           ),
         nlfs <-
           read_with_default(
             node_id,
             id,
             transparent_keyspace("nullifiers"),
             MapSet.new()
           ),
         # possibly also add check for CU roots
         true <- storage_check(tx, cms, nlfs),
         roots <-
           read_with_default(node_id, id, transparent_keyspace("roots"), []),
         true <- verify_tx_root(tx, roots) do
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

      writes = [
        {transparent_keyspace("roots"),
         [
           TAcc.value(
             MapSet.union(
               map.commitments,
               cms
             )
           )
           | roots
         ]},
        {transparent_keyspace("nullifiers"),
         MapSet.union(nlfs, map.nullifiers)},
        {transparent_keyspace("commitments"),
         MapSet.union(cms, map.commitments)}
      ]

      Ordering.write(node_id, {id, writes ++ map.blobs})
      transparent_rm_event(map.commitments, map.nullifiers, node_id)

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

  @spec verify_tx_root(TTransaction.t(), list(Noun.t())) ::
          true | {:error, String.t()}
  defp verify_tx_root(trans = %TTransaction{}, roots) do
    with true <- roots_exist?(trans, roots) do
      true
    else
      {:error, msg} ->
        {:error, "Root does not exist: " <> msg}
    end
  end

  @spec storage_check(TTransaction.t(), MapSet.t(), MapSet.t()) ::
          true | {:error, String.t()}
  defp storage_check(trans, stored_commitments, stored_nullifiers) do
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

  @spec roots_exist?(TTransaction.t(), list(Noun.t())) ::
          true | {:error, String.t()}
  defp roots_exist?(
         trans = %TTransaction{},
         committed_roots
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
      if Enum.any?(committed_roots, &Noun.equal?(&1, root)) do
        {:cont, acc}
      else
        {:halt, {:error, "Root #{inspect(root)} is absent"}}
      end
    end)
  end

  @spec emit_value(String.t(), binary(), :error | Noun.t()) ::
          {:ok, Noun.t()} | :error
  def emit_value(node_id, id, result) do
    wrapped_result =
      case result do
        :error -> :error
        res -> {:ok, res}
      end

    event =
      Node.Event.new_with_body(node_id, %Events.ROEvent{
        tx_id: id,
        read_result: wrapped_result
      })

    EventBroker.event(event)
    wrapped_result
  end

  @spec store_value(String.t(), binary(), Noun.t()) :: {:ok, any} | :error
  def store_value(node_id, id, result) do
    with {:ok, list} <- result |> Noun.list_nock_to_erlang_safe(),
         true <-
           Enum.all?(list, fn
             [_ | _] -> true
             _ -> false
           end) do
      for [k | v] <- list do
        Ordering.write(
          node_id,
          {id, [{k, v}]}
        )
      end

      {:ok, list}
    else
      _ -> :error
    end
  end

  @spec empty_write(backend(), String.t(), binary()) :: :ok
  defp empty_write(:read_only, _node_id, _id) do
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
         old_roots <-
           read_with_default(
             node_id,
             id,
             cairo_keyspace("roots"),
             MapSet.new()
           ),
         old_nlfs <-
           read_with_default(
             node_id,
             id,
             cairo_keyspace("nullifiers"),
             MapSet.new()
           ),
         true <- root_existence_check(tx, old_roots),
         # No need to check the commitment existence
         true <- nullifier_existence_check(tx, old_nlfs) do
      {ct, append_roots} =
        case Ordering.read(node_id, {id, cairo_keyspace("ct")}) do
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
              {["anoma", "blob", :crypto.hash(:sha256, value)], value}
            end)
          end)
        end)

      old_ciphertexts =
        read_with_default(
          node_id,
          id,
          cairo_keyspace("ciphertexts"),
          MapSet.new()
        )

      writes = [
        {cairo_keyspace("nullifiers"), MapSet.union(old_nlfs, nullifiers)},
        {cairo_keyspace("roots"),
         MapSet.put(append_roots, anchor) |> MapSet.union(old_roots)},
        {cairo_keyspace("ciphertexts"),
         MapSet.union(ciphertexts, old_ciphertexts)},
        {cairo_keyspace("ct"), ct_new} | write_app_data
      ]

      Ordering.write(node_id, {id, writes})

      cairo_rm_event(
        MapSet.new(commitments),
        nullifiers,
        node_id
      )

      {:ok, tx}
    else
      e ->
        Logging.log_event(
          node_id,
          :error,
          "Transaction verification failed. Reason: #{inspect(e)}"
        )

        :error
    end
  end

  @spec nullifier_existence_check(CTransaction.t(), MapSet.t(binary())) ::
          true | {:error, String.t()}
  def nullifier_existence_check(transaction, stored_nullifiers) do
    if Enum.any?(
         CTransaction.nullifiers(transaction),
         &MapSet.member?(stored_nullifiers, &1)
       ) do
      {:error, "A submitted nullifier already exists in storage"}
    else
      true
    end
  end

  @spec root_existence_check(CTransaction.t(), MapSet.t(binary())) ::
          true | {:error, String.t()}
  def root_existence_check(transaction, stored_roots) do
    Enum.all?(transaction.roots, &MapSet.member?(stored_roots, &1)) or
      {:error, "A submitted root dose not exist in storage"}
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
      Node.Event.new_with_body(node_id, %Events.CompleteEvent{
        tx_id: id,
        tx_result: result
      })

    event(backend, event)
  end

  @spec result_event(String.t(), any(), String.t(), backend()) :: :ok
  defp result_event(id, result, node_id, backend) do
    event =
      Node.Event.new_with_body(node_id, %Events.ResultEvent{
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
      Node.Event.new_with_body(node_id, %Events.TRMEvent{
        commitments: cms,
        nullifiers: nlfs
      })

    EventBroker.event(event)
  end

  @spec cairo_rm_event(
          MapSet.t(binary()),
          MapSet.t(binary()),
          String.t()
        ) :: :ok
  defp cairo_rm_event(cms, nlfs, node_id) do
    event =
      Node.Event.new_with_body(node_id, %Events.SRMEvent{
        commitments: cms,
        nullifiers: nlfs
      })

    EventBroker.event(event)
  end

  @spec event(backend(), EventBroker.Event.t()) :: :ok
  defp event(:read_only, _event) do
    :ok
  end

  defp event(_backend, event) do
    EventBroker.event(event)
  end

  @spec read_with_default(String.t(), binary(), list(), any()) :: any()
  defp read_with_default(node_id, tx_id, key, default) do
    case Ordering.read(node_id, {tx_id, key}) do
      :absent -> default
      {:ok, val} -> val
    end
  end

  @spec cairo_keyspace(String.t()) :: list(String.t())
  defp cairo_keyspace(key) do
    anoma_keyspace(["cairo", key])
  end

  @spec transparent_keyspace(String.t()) :: list(String.t())
  defp transparent_keyspace(key) do
    anoma_keyspace(["transparent", key])
  end

  @spec anoma_keyspace(list(String.t())) :: list(String.t())
  defp anoma_keyspace(key) do
    ["anoma" | key]
  end
end
