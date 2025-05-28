defmodule Anoma.Node.Examples.IntraNode do
  alias Anoma.Node.Examples.EAdvertise
  alias Anoma.Node.Examples.ETransaction
  alias Anoma.Node.Transaction.Mempool

  import ExUnit.Assertions

  @doc """
  I start two nodes that advertise to eachother, and then I request a mempool dump from the remote node.
  """
  @spec intranode_tx_dump :: :ok
  def intranode_tx_dump do
    # create two nodes in a distributed setting and advertise them to eachother.
    {_local, remote, slave} =
      EAdvertise.seed_nodes_distributed(stop_slave: false)

    # at this point, the remote node is known, and we should be able to get information from its mempool.
    assert [] == Mempool.tx_dump(remote.node_id)

    # create a random transaction to add
    transaction = ETransaction.simple_transaction()

    Mempool.tx(
      remote.node_id,
      {transaction.backend, transaction.noun},
      transaction.id
    )

    # assert that the transaction is in the node
    assert Mempool.tx_dump(remote.node_id) == [transaction.id]

    # stop the slave
    EAdvertise.stop_slave(slave)
    :ok
  end
end
