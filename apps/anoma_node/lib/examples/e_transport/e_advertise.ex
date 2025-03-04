defmodule Anoma.Node.Examples.EAdvertise do
  alias Anoma.Node.Examples.ENode
  alias Anoma.Node.Transport.NetworkRegister.Advert
  alias Anoma.Node.Transport.NetworkRegister.Advert.GRPCAddress
  alias Anoma.Node.Transport.NetworkRegister
  alias Anoma.Node.Transport.GRPC.Advertise
  alias Anoma.Node.Config
  import ExUnit.Assertions

  @doc """
  I create two nodes, and I advertise the first node to the second node. I do
  this by sending a GRPC request that advertises the first node to the second
  node.
  """
  def advertise_node() do
    # node 1
    cfg1 = Config.node()
    node_1 = ENode.start_noded(node_config: cfg1)

    # node 2
    cfg2 = Config.node()
    node_2 = ENode.start_noded(node_config: cfg2)

    address = %GRPCAddress{
      host: cfg1.instance.node_grpc_host,
      port: cfg1.instance.node_grpc_port
    }

    Advertise.advertise(cfg1, cfg2.node_id, %Advert{
      node_id: cfg1.node_id,
      grpc_address: address,
      version: "unknown"
    })

    # assert that node 2's network register know node 1.
    node_2_register = NetworkRegister.dump_register(cfg2.node_id)
    assert Map.get(node_2_register.nodes, cfg1.node_id) != nil

    # since node 2 knows node 1, it can advertise directly without manually crafting the message.
    NetworkRegister.advertise_to(cfg2.node_id, cfg1.node_id)

    # return the nodes
    {node_1, node_2}
  end

  def seed_nodes() do
    # node 1
    cfg1 = Config.node()
    node_1 = ENode.start_noded(node_config: cfg1)

    # create an advert for node 1 to store in the seed list of node 2
    address = %GRPCAddress{
      host: cfg1.instance.node_grpc_host,
      port: cfg1.instance.node_grpc_port
    }

    node2_info = %Advert{
      node_id: cfg1.node_id,
      grpc_address: address,
      version: "unknown"
    }

    # node 2
    cfg2 =
      Config.node() |> Map.put(:seed_nodes, %{cfg1.node_id => node2_info})

    node_2 = ENode.start_noded(node_config: cfg2)

    # assert that node 1's network register know node 2.
    node_1_register = NetworkRegister.dump_register(cfg1.node_id)
    assert Map.get(node_1_register.nodes, cfg2.node_id) != nil

    # return the nodes
    {node_1, node_2}
  end
end
