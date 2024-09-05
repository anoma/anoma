defmodule Anoma.Node.Mempool.Backends do
   @moduledoc """
   Backend module.

   Support :kv, :ro, :rm, :cairo execution.
   """

  alias Anoma.Resource.Transaction, as: TTransaction
  alias Anoma.RM.Transaction
  alias Anoma.ShieldedResource.ShieldedTransaction
  alias Anoma.Node.{Storage, Ordering, Logger, Router}

  import Nock
  require Noun

  @type backend() :: :kv | :rm | :cairo | :ro

  @type transaction() :: {backend(), Noun.t() | binary()}

  def execute({backend, tx}, env, id, reply_to)
       when Noun.is_noun_atom(tx) do
    case Nock.Cue.cue(tx) do
      {:ok, tx} ->
        execute({backend, tx}, env)

      :error ->
        log_info({:fail, "failed to cue!", env.logger})
        :error
    end
  end

  def execute({:ro, tx}, ord, id, reply_to) do
    execute_kv_ro(tx, ord, id, reply_to, &send_value/3)
  end

  def execute({:kv, tx}, ord, id, nil) do
    execute_kv_ro(tx, ord, id, reply_to, &store_value/3)
  end

  def execute({:rm, tx}, ord, id, nil) do
    execute_rm_cairo(tx, ord, id, reply_to, TTransaction)
  end

  def execute({:cairo, tx}, ord, id, nil) do
    execute_rm_cairo(tx, ord, id, reply_to, ShieldedTransaction)
  end

  defp gate_call(tx_code, env, id) do
    with {:ok, stage_2_tx} <- nock(proto_tx, [9, 2, 0 | 1], env),
         {:ok, ordered_tx} <-
    nock(stage_2_tx, [10, [6, 1 | id], 0 | 1], env),
    {:ok, result} <- nock(ordered_tx, [9, 2, 0 | 1], env)
      do
      {:ok, result}
      else
        e -> e
    end
  end

  defp execute_kv_ro(tx_code, env, id, reply_to, process) do
    with {:ok, result} <- gate_call(tx_code, env, id),
         :ok <- process.(result, env, reply_to) do
      log_info({:success_run, logger})
      {:ok, result}
    else
      e ->
        log_info({:fail, e, logger})
        :error
    end
  end

  defp send_value(result, env, reply_to) do
    # send the value to reply-to address and the topic
    reply_msg = {:read_value, value}
    send_if_addr(reply_to, reply_msg)

    log_info({:get, value, env.logger})
  end

  defp store_value(result, env, reply_to) do
    with [key | value] <- result do
      true_order = Ordering.read(pid, key)
      logger = env.logger
      log_info({:writing, true_order, logger})
      Ordering.write(pid, key, value)
      log_info({:put, key, logger})
      :ok
    else
      e -> e
    end
  end

  defp execute_rm_cairo(tx_code, env, id, reply_to, process) do
    logger = env.logger

    log_info({:dispatch, id, logger}
    with
         {:ok, resource_tx} <- gate_call(gate, env)
         {:ok, vm_resource_tx} <- mod.from_noun(resource_tx),
         true_order = wait_for_ready(s),
         true <- Transaction.verify(vm_resource_tx),
         true <-
           Transaction.resource_existence_check(vm_resource_tx, storage),
         true <-
           rm_nullifier_check(
             storage,
             Transaction.storage_nullifiers(vm_resource_tx)
           ) do
             persist(env, true_order, vm_resource_tx)
             {:ok, vm_resource_tx}
    else
      # The failure had to be on the true match above, which is after
      # the wait for ready
      false ->
        log_info({:fail, false, logger})
        snapshot(storage, env)
        :error

      # This failed before the waiting for read as it's likely :error
      e ->
        log_info({:fail, e, logger})
        wait_for_ready(s)
        snapshot(storage, env)
        :error
    end
  end

  @spec persist(Nock.t(), Noun.t(), Transaction.t()) :: any()
  defp persist(env, true_order, vm_resource_tx) do
    logger = env.logger
    storage = Router.Engine.get_state(env.ordering).storage

    log_info({:writing, true_order, logger})
    # this is not quite correct, but storage has to be revisited as a whole
    # for it to be made correct.
    # in particular, the get/put api must be deleted, since it cannot be correct,
    # but an append api should also be added.
    # the latter requires the merkle tree to be complete
    cm_tree = Transaction.cm_tree(vm_resource_tx, storage)

    commitments = Transaction.storage_commitments(vm_resource_tx)

    for commitment <- commitments do
      cm_key = ["rm", "commitments", commitment]

      Storage.put(storage, cm_key, true)
      log_info({:put, cm_key, logger})
    end

    {_ct, anchor} = CommitmentTree.add(cm_tree, commitments)

    Storage.put(storage, ["rm", "commitment_root", anchor], true)

    for nullifier <- Transaction.storage_nullifiers(vm_resource_tx) do
      nf_key = ["rm", "nullifiers", nullifier]
      Storage.put(storage, nf_key, true)
      log_info({:put, nf_key, logger})
    end

    snapshot(storage, env)
    log_info({:success_run, logger})
    :ok
  end

  ############################################################
  #                     Conceptual Helpers                   #
  ############################################################

  @doc """
  I perform the nullifier check for a resource machine transaction.

  Given a storage and a list of stored nullifiers I check their placing in storage.
  """
  @spec rm_nullifier_check(Router.addr(), list(binary())) :: bool()
  def rm_nullifier_check(storage, nullifiers) do
    for nullifier <- nullifiers, reduce: true do
      acc ->
        nf_key = ["rm", "nullifiers", nullifier]
        acc && Storage.get(storage, nf_key) == :absent
    end
  end

  @spec snapshot(Router.addr(), Nock.t()) ::
          :ok | nil
  defp snapshot(storage, env) do
    snapshot = hd(env.snapshot_path)
    log_info({:snap, {storage, snapshot}, env.logger})
    Storage.put_snapshot(storage, snapshot)
  end

  @spec send_if_addr(Router.addr() | nil, any()) :: :ok | nil
  defp send_if_addr(addr, msg) do
    if addr do
      Router.cast(addr, msg)
    end
  end
end
