defmodule Anoma.Node.Executor.Worker do
  @moduledoc """
  I am a Nock worker, supporting scry.
  """
  alias Anoma.Storage
  alias Anoma.Node.Storage.Ordering
  alias Anoma.Node.Logger

  import Nock

  @spec run(Noun.t(), {:kv | :rm, Noun.t()}, Nock.t()) :: :ok | :error
  def run(order, {:kv, proto_tx}, env) do
    logger = env.logger

    log_info({:dispatch, order, logger})
    storage = Ordering.get_storage(env.ordering)

    with {:ok, stage_2_tx} <- nock(proto_tx, [9, 2, 0 | 1], env),
         {:ok, ordered_tx} <-
           nock(stage_2_tx, [10, [6, 1 | order], 0 | 1], env),
         {:ok, [key | value]} <- nock(ordered_tx, [9, 2, 0 | 1], env) do
      true_order = wait_for_ready(env, order)

      log_info({:writing, true_order, logger})
      Storage.put(storage, key, value)
      log_info({:put, key, logger})
      snapshot(storage, env)
      log_info({:success_run, logger})
      :ok
    else
      e ->
        log_info({:fail, e, logger})
        wait_for_ready(env, order)
        snapshot(storage, env)
        :error
    end
  end

  def run(order, {:rm, gate}, env) do
    logger = env.logger

    log_info({:dispatch, order, logger})
    storage = Ordering.get_storage(env.ordering)

    with {:ok, ordered_tx} <- nock(gate, [10, [6, 1 | order], 0 | 1], env),
         {:ok, resource_tx} <- nock(ordered_tx, [9, 2, 0 | 1], env),
         vm_resource_tx <- Anoma.Resource.Transaction.from_noun(resource_tx),
         true <- Anoma.Resource.Transaction.verify(vm_resource_tx),
         true <- rm_nullifier_check(storage, vm_resource_tx.nullifiers) do
      true_order = wait_for_ready(env, order)

      log_info({:writing, true_order, logger})
      # this is not quite correct, but storage has to be revisited as a whole
      # for it to be made correct.
      # in particular, the get/put api must be deleted, since it cannot be correct,
      # but an append api should also be added.
      # the latter requires the merkle tree to be complete
      for commitment <- vm_resource_tx.commitments do
        cm_key = ["rm", "commitments", commitment]
        Storage.put(storage, cm_key, true)
        log_info({:put, cm_key, logger})
      end

      for nullifier <- vm_resource_tx.nullifiers do
        nf_key = ["rm", "nullifiers", nullifier]
        Storage.put(storage, nf_key, true)
        log_info({:put, nf_key, logger})
      end

      snapshot(storage, env)
      log_info({:success_run, logger})
      :ok
    else
      e ->
        log_info({:fail, e, logger})
        wait_for_ready(env, order)
        snapshot(storage, env)
        :error
    end
  end

  @spec rm_nullifier_check(Storage.t(), list(binary())) :: bool()
  def rm_nullifier_check(storage, nullifiers) do
    for nullifier <- nullifiers, reduce: true do
      acc ->
        nf_key = ["rm", "nullifiers", nullifier]
        acc && Storage.get(storage, nf_key) == :absent
    end
  end

  @spec wait_for_ready(Nock.t(), Noun.t()) :: any()
  def wait_for_ready(env, order) do
    logger = env.logger

    log_info({:ensure_read, logger})

    Ordering.caller_blocking_read_id(
      env.ordering,
      [order | env.snapshot_path]
    )

    log_info({:waiting_write_ready, logger})

    receive do
      {:write_ready, order} ->
        log_info({:write_ready, logger})
        order
    end
  end

  @spec snapshot(Storage.t(), Nock.t()) :: {:aborted, any()} | {:atomic, :ok}
  def snapshot(storage, env) do
    snapshot = hd(env.snapshot_path)
    log_info({:snap, {storage, snapshot}, env.logger})
    Storage.put_snapshot(storage, snapshot)
  end

  ############################################################
  #                     Logging Info                         #
  ############################################################

  defp log_info({:dispatch, order, logger}) do
    Logger.add(logger, :info, "Worker dispatched.
    Order id: #{inspect(order)}")
  end

  defp log_info({:writing, order, logger}) do
    Logger.add(logger, :info, "Worker writing.
    True order: #{inspect(order)}")
  end

  defp log_info({:fail, error, logger}) do
    Logger.add(logger, :error, "Worker failed! #{inspect(error)}")
  end

  defp log_info({:put, key, logger}) do
    Logger.add(logger, :info, "Putting #{inspect(key)}")
  end

  defp log_info({:success_run, logger}) do
    Logger.add(logger, :info, "Run succesfull!")
  end

  defp log_info({:ensure_read, logger}) do
    Logger.add(
      logger,
      :info,
      "#{inspect(self())}: making sure the snapshot is ready"
    )
  end

  defp log_info({:waiting_write_ready, logger}) do
    Logger.add(
      logger,
      :info,
      "#{inspect(self())}: waiting for a write ready"
    )
  end

  defp log_info({:write_ready, logger}) do
    Logger.add(
      logger,
      :info,
      "#{inspect(self())}: write ready"
    )
  end

  defp log_info({:snap, {s, ss}, logger}) do
    Logger.add(
      logger,
      :info,
      "Taking snapshot key #{inspect(ss)} in storage #{inspect(s)}"
    )
  end
end
