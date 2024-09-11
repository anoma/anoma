defmodule Anoma.Node.Mempool.Backends do
  @moduledoc """
  Backend module.
  Support :kv, :ro, :rm, :cairo execution.
  """

  alias Anoma.Resource.Transaction, as: TTransaction
  alias Anoma.RM.Transaction
  alias Anoma.ShieldedResource.ShieldedTransaction
  alias Anoma.Node.Mempool.Ordering

  import Nock
  require Noun
  require EventBroker.Event
  use TypedStruct

  @type backend() :: :kv | :rm | :cairo | :ro

  @type transaction() :: {backend(), Noun.t() | binary()}

  typedstruct enforce: true, module: CompleteEvent do
    field(:id, integer())
    field(:result, :ok | :error)
  end

  def execute({backend, tx}, env, id, reply_to)
      when Noun.is_noun_atom(tx) do
    case Nock.Cue.cue(tx) do
      {:ok, tx} ->
        execute({backend, tx}, env, id, reply_to)

      :error ->
        :error
    end
  end

  def execute({:ro, tx}, id, reply_to) do
    execute_kv_ro(tx, id, reply_to, &send_value/3)
  end

  def execute({:kv, tx}, id, nil) do
    execute_kv_ro(tx, id, nil, &store_value/3)
  end

  # def execute({:rm, tx}, id, nil) do
  #   execute_rm_cairo(tx, id, TTransaction)
  # end

  # def execute({:cairo, tx}, id, nil) do
  #   execute_rm_cairo(tx, id, ShieldedTransaction)
  # end

  defp gate_call(tx_code, env, id) do
    with {:ok, ordered_tx} <- nock(tx_code, [10, [6, 1 | id], 0 | 1], env),
         {:ok, result} <- nock(ordered_tx, [9, 2, 0 | 1], env) do
      {:ok, result}
    else
      e -> e
    end
  end

  defp execute_kv_ro(tx_code, id, reply_to, process) do
    env = %Nock{}

    with {:ok, stage_2_tx} <- nock(tx_code, [9, 2, 0 | 1], env),
         {:ok, result} <- gate_call(stage_2_tx, env, id),
         :ok <- process.(id, result, reply_to) do
      ok_event =
        EventBroker.Event.new_with_body(%__MODULE__.CompleteEvent{
          id: id,
          result: :ok
        })

      EventBroker.event(ok_event)
    else
      _e ->
        error_event =
          EventBroker.Event.new_with_body(%__MODULE__.CompleteEvent{
            id: id,
            result: :error
          })

        EventBroker.event(error_event)
    end
  end

  defp send_value(_id, result, reply_to) do
    # send the value to reply-to address and the topic
    reply_msg = {:read_value, result}
    send_if_addr(reply_to, reply_msg)
  end

  defp store_value(id, result, _reply_to) do
    with [key | value] <- result do
      Ordering.write({id, key}, value)
      IO.puts("===========Worker concluded writing succesfully========")
      :ok
    else
      e -> e
    end
  end

  # defp execute_rm_cairo(tx_code, env, id, _reply_to, mod) do
  #   with {:ok, resource_tx} <- gate_call(tx_code, env),
  #        {:ok, vm_resource_tx} <- mod.from_noun(resource_tx),
  #        true <- Transaction.verify(vm_resource_tx),
  #        true <-
  #          Transaction.resource_existence_check(vm_resource_tx, storage),
  #        true <-
  #          rm_nullifier_check(
  #            storage,
  #            Transaction.storage_nullifiers(vm_resource_tx)
  #          ) do
  #     persist(env, id, vm_resource_tx)
  #     {:ok, vm_resource_tx}
  #   else
  #     # The failure had to be on the true match above, which is after
  #     # the wait for ready
  #     false ->
  #       :error

  #     # This failed before the waiting for read as it's likely :error
  #     e ->
  #       :error
  #   end
  # end

  # @spec persist(Nock.t(), Noun.t(), Transaction.t()) :: any()
  # defp persist(env, true_order, vm_resource_tx) do
  #   logger = env.logger
  #   storage = Router.Engine.get_state(env.ordering).storage

  #   # this is not quite correct, but storage has to be revisited as a whole
  #   # for it to be made correct.
  #   # in particular, the get/put api must be deleted, since it cannot be correct,
  #   # but an append api should also be added.
  #   # the latter requires the merkle tree to be complete
  #   cm_tree = Transaction.cm_tree(vm_resource_tx, storage)

  #   commitments = Transaction.storage_commitments(vm_resource_tx)

  #   for commitment <- commitments do
  #     cm_key = ["rm", "commitments", commitment]

  #     Storage.put(storage, cm_key, true)
  #   end

  #   {_ct, anchor} = CommitmentTree.add(cm_tree, commitments)

  #   Storage.put(storage, ["rm", "commitment_root", anchor], true)

  #   for nullifier <- Transaction.storage_nullifiers(vm_resource_tx) do
  #     nf_key = ["rm", "nullifiers", nullifier]
  #     Storage.put(storage, nf_key, true)
  #   end
  #   :ok
  # end

  ############################################################
  #                     Conceptual Helpers                   #
  ############################################################

  # @doc """
  # I perform the nullifier check for a resource machine transaction.
  # Given a storage and a list of stored nullifiers I check their placing in storage.
  # """
  # def rm_nullifier_check(storage, nullifiers) do
  #   for nullifier <- nullifiers, reduce: true do
  #     acc ->
  #       nf_key = ["rm", "nullifiers", nullifier]
  #       acc && Ordering.read({id, nf_key}) == :absent
  #   end
  # end

  @spec send_if_addr(Router.addr() | nil, any()) :: :ok | nil
  defp send_if_addr(addr, msg) do
    if addr do
      Router.cast(addr, msg)
    end
  end
end
