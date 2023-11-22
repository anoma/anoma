defmodule Anoma.Node.Worker do
  @moduledoc """
  I am a Nock worker, supporting scry.
  """

  @dialyzer :no_improper_lists

  import Nock

  def run(order, gate) do
    IO.inspect(order, label: "worker dispatched with order id")
    {:ok, ordered_tx} = nock(gate, [10, [6, 1 | order], 0 | 1])
    tx_result = nock(ordered_tx, [9, 2, 0 | 1])

    case tx_result do
      {:ok, [key | value]} ->
        IO.inspect(tx_result, label: "worker succeeded with output")
        # wait to be told to make writes
        true_order =
          receive do
            {:write_ready, order} ->
              IO.inspect(self(), label: "got write ready")
              order
          end

        IO.inspect(true_order, label: "worker writing at true order")
        Anoma.Node.Storage.put(key, value)

      _ ->
        IO.puts("worker failed!")
        IO.inspect(tx_result, label: "failure result")
    end
  end
end
