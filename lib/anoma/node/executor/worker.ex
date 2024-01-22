defmodule Anoma.Node.Executor.Worker do
  @moduledoc """
  I am a Nock worker, supporting scry.
  """
  alias Anoma.Storage
  alias Anoma.Node.Storage.{Communicator, Ordering}

  import Nock
  require Logger

  @spec run(Noun.t(), {:kv | :rm, Noun.t()}, Nock.t()) :: :ok | :error
  def run(order, {:kv, gate}, env) do
    instrument({:dispatch, order})
    storage = Communicator.get_storage(env.ordering)

    with {:ok, ordered_tx} <- nock(gate, [10, [6, 1 | order], 0 | 1], env),
         {:ok, [key | value]} <- nock(ordered_tx, [9, 2, 0 | 1], env) do
      true_order = wait_for_ready(env, order)

      instrument({:writing, true_order})
      Storage.put(storage, key, value)
      snapshot(storage, env)
      :ok
    else
      _ ->
        instrument(:fail)
        wait_for_ready(env, order)
        snapshot(storage, env)
        :error
    end
  end

  def run(order, {:rm, gate}, env) do
    instrument({:dispatch, order})
    storage = Communicator.get_storage(env.ordering)

    with {:ok, ordered_tx} <- nock(gate, [10, [6, 1 | order], 0 | 1], env),
         {:ok, resource_tx} <- nock(ordered_tx, [9, 2, 0 | 1], env),
         vm_resource_tx <- Anoma.Resource.Transaction.from_noun(resource_tx),
         true <- Anoma.Resource.Transaction.verify(vm_resource_tx) do
      true_order = wait_for_ready(env, order)

      instrument({:writing, true_order})
      # this is not quite correct, but storage has to be revisited as a whole
      # for it to be made correct.
      # in particular, the get/put api must be deleted, since it cannot be correct,
      # but an append api should also be added.
      # the latter requires the merkle tree to be complete
      for commitment <- vm_resource_tx.commitments do
        cm_key = ["rm", "commitments", commitment]
        Storage.put(storage, cm_key, true)
      end

      for nullifier <- vm_resource_tx.nullifiers do
        nf_key = ["rm", "nullifiers", nullifier]
        Storage.put(storage, nf_key, true)
      end

      snapshot(storage, env)
      :ok
    else
      _ ->
        instrument(:fail)
        wait_for_ready(env, order)
        snapshot(storage, env)
        :error
    end
  end

  @spec wait_for_ready(Nock.t(), Noun.t()) :: any()
  def wait_for_ready(env, order) do
    instrument(:ensure_read)

    Ordering.caller_blocking_read_id(
      env.ordering,
      [order | env.snapshot_path]
    )

    instrument(:waiting_write_ready)

    receive do
      {:write_ready, order} ->
        instrument(:write_ready)
        order
    end
  end

  @spec snapshot(Storage.t(), Nock.t()) :: {:aborted, any()} | {:atomic, :ok}
  def snapshot(storage, env) do
    snapshot = hd(env.snapshot_path)
    instrument({:snapshot, {storage, snapshot}})
    Storage.put_snapshot(storage, snapshot)
  end

  defp instrument({:dispatch, d}) do
    Logger.info("worker dispatched with order id: #{inspect(d)}")
  end

  defp instrument(:write_ready) do
    Logger.info("#{inspect(self())}: write ready")
  end

  defp instrument(:ensure_read) do
    Logger.info("#{inspect(self())}: making sure the snapshot is ready")
  end

  defp instrument(:waiting_write_ready) do
    Logger.info("#{inspect(self())}: waiting for a write ready")
  end

  defp instrument({:writing, ord}) do
    Logger.info("Worker writing at true order #{inspect(ord)}")
  end

  defp instrument(:fail) do
    Logger.warning("#{inspect(self())}：Worker failed")
  end

  defp instrument({:snapshot, {s, ss}}) do
    Logger.debug(
      "Taking snapshot key #{inspect(ss)} in storage #{inspect(s)}"
    )
  end
end
