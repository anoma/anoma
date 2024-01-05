defmodule Anoma.Node.Executor.Worker do
  @moduledoc """
  I am a Nock worker, supporting scry.
  """
  alias Anoma.Storage
  alias Anoma.Node.Storage.{Communicator, Ordering}

  import Nock

  @spec run(Noun.t(), Noun.t(), Nock.t()) :: :ok | :error
  def run(order, gate, env) do
    instrument(env.instrumentation, {:dispatch, order})
    storage = Communicator.get_storage(env.ordering)

    with {:ok, ordered_tx} <- nock(gate, [10, [6, 1 | order], 0 | 1], env),
         {:ok, [key | value]} <- nock(ordered_tx, [9, 2, 0 | 1], env) do
      true_order = wait_for_ready(env, order)

      instrument(env.instrumentation, {:writing, true_order})
      Storage.put(storage, key, value, env.instrumentation)
      snapshot(storage, env)
      :ok
    else
      _ ->
        instrument(env.instrumentation, :fail)
        wait_for_ready(env, order)
        snapshot(storage, env)
        :error
    end
  end

  @spec wait_for_ready(Nock.t(), Noun.t()) :: any()
  def wait_for_ready(env, order) do
    instrumentation = env.instrumentation
    instrument(instrumentation, :ensure_read)

    Ordering.caller_blocking_read_id(
      env.ordering,
      [order | env.snapshot_path],
      instrumentation
    )

    instrument(instrumentation, :waiting_write_ready)

    receive do
      {:write_ready, order} ->
        instrument(instrumentation, :write_ready)
        order
    end
  end

  @spec snapshot(Storage.t(), Nock.t()) :: {:aborted, any()} | {:atomic, :ok}
  def snapshot(storage, env) do
    snapshot = hd(env.snapshot_path)
    instrument(env.instrumentation, {:snapshot, {storage, snapshot}})
    Storage.put_snapshot(storage, snapshot)
  end

  defp instrument(instrument, {:dispatch, d}) do
    if instrument, do: IO.inspect(d, label: "worker dispatched with order id")
  end

  defp instrument(instrument, :write_ready) do
    if instrument, do: IO.inspect(self(), label: "got write ready")
  end

  defp instrument(instrument, :ensure_read) do
    if instrument,
      do: IO.inspect(self(), label: "Making sure our snapshot is ready")
  end

  defp instrument(instrument, :waiting_write_ready) do
    if instrument, do: IO.inspect(self(), label: "waiting for a write ready")
  end

  defp instrument(instrument, {:writing, ord}) do
    if instrument, do: IO.inspect(ord, label: "worker writing at true order")
  end

  defp instrument(instrument, :fail) do
    if instrument, do: IO.puts("Worker Failed")
  end

  defp instrument(instrument, {:snapshot, {s, ss}}) do
    if instrument, do: IO.inspect({s, ss}, label: "Taking snapshot")
  end
end
