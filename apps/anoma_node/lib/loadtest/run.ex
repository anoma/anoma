defmodule Anoma.Node.Loadtest.Run do
  alias Anoma.Client
  alias Examples.ETransparent.ETransaction

  import Helpers

  @doc """
  I submit a number of transactions to the mempool via the client and measure
  how long it takes for them to be processed.
  """
  def measure_average_transaction_time(count) do
    average_measure(
      fn ->
        Client.add_transaction(ETransaction.swap_from_actions_random())
      end,
      count
    )
  end

  def run() do
    {:ok, pid} = Agent.start(fn -> 0 end)
    :global.register_name(:agent, pid)

    # create a node
    node = Peer.create_node()

    this = self()

    client_count = 20
    iterations = 1000

    clients = for _ <- 1..client_count, do: Peer.create_client(node)

    for client <- clients do
      spawn(fn ->
        {average_time_μs, _} =
          :rpc.block_call(
            client.name,
            Anoma.Node.Loadtest.Run,
            :measure_average_transaction_time,
            [iterations]
          )

        # if the average time for a transaction is `average_time`,
        # the node can process `1_000_000 / average_time` transactions per second
        transactions_per_second = 1_000_000 / average_time_μs

        send(this, transactions_per_second)
      end)
    end

    for _ <- 1..client_count do
      receive do
        i -> i
      end
    end
  end
end
