defmodule Anoma.Node.Worker do
  @moduledoc """
  I am a Nock worker, supporting scry.
  """

  @dialyzer :no_improper_lists

  import Nock

  def run(order, gate) do
    IO.inspect(order, label: "worker dispatched after order")
    {:ok, ordered_tx} = nock(gate, [10, [6, 1 | order], 0 | 1])
    tx_result = nock(ordered_tx, [9, 2, 0 | 1])

    case tx_result do
      {:ok, [key | value]} ->
        Anoma.Node.Storage.put(key, value)
        IO.puts("worker succeeded")

      _ ->
        IO.puts("worker failed!")
    end
  end
end
