defmodule Introspect do
  @moduledoc """
  Functions that return information about the running system.
  """

  @spec mempool(%{node_id: binary(), name: atom()}) :: :ok
  def mempool(node) do
    # get the total transactions in the mempool
    Node.spawn(node.name, fn ->
      pid = Anoma.Node.Registry.whereis(node.node_id, Anoma.Node.Transaction.Mempool)
      state = :sys.get_state(pid)

      IO.puts("""
                  round: #{inspect(state.round)}
      transaction count: #{inspect(Enum.count(state.transactions))}
      """)
    end)
  end
end
