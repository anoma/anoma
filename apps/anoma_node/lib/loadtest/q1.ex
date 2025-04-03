defmodule Q1 do
  alias Examples.ETransparent.ETransaction
  alias Anoma.Client

  def run(client) do
    # submit a transaction
    # run the action on the client node
    Node.spawn(client.name, fn ->
      transaction = ETransaction.swap_from_actions_random()
      Client.add_transaction(ETransaction.swap_from_actions_random())
    end)
  end
end
