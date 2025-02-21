defmodule Anoma.Node.Examples.EAdvertise do
  alias Anoma.Node.Examples.ENode
  alias Anoma.Node.Transport.NetworkRegister.Advert
  alias Anoma.Node.Transport.NetworkRegister.Advert.GRPCAddress
  alias Anoma.Node.Transport.NetworkRegister
  alias Anoma.Node.Transport.GRPC.Advertise

  import ExUnit.Assertions

  # @doc """
  # I create a config for a node based on its node id.
  # """
  @spec node_config(String.t()) :: map()
  defp node_config(node_id \\ "#{:erlang.phash2(make_ref())}") do
    grpc_host = "localhost"
    grpc_port = Application.get_env(:anoma_node, :grpc_port)

    %{
      grpc_port: grpc_port,
      grpc_host: grpc_host,
      seed_nodes: %{},
      node_id: node_id
    }
  end

  @doc """
  I create two nodes, and I advertise the first node to the second node. I do
  this by sending a GRPC request that advertises the first node to the second
  node.
  """
  def advertise_node() do
    # node 1
    cfg1 = node_config()
    node_1 = ENode.start_node(node_config: cfg1, node_id: cfg1.node_id)

    # node 2
    cfg2 = node_config()
    node_2 = ENode.start_node(node_id: cfg2.node_id, node_config: cfg2)

    # manually advertise node 1 to node 2 via GRPC
    # this address is the same for both nodes since they run in the same VM.
    grpc_port = Application.get_env(:anoma_node, :grpc_port)
    address = %GRPCAddress{host: "localhost", port: grpc_port}

    Advertise.advertise(cfg1, cfg2.node_id, %Advert{
      node_id: cfg1.node_id,
      grpc_address: address,
      version: "unknown"
    })

    # assert that node 2's network register know node 1.
    node_2_register = NetworkRegister.dump_register(node_2.node_id)
    assert Map.get(node_2_register.nodes, node_1.node_id) != nil

    # since node 2 knows node 1, it can advertise directly without manually crafting the message.
    NetworkRegister.advertise_to(node_2.node_id, node_1.node_id)

    # return the nodes
    {node_1, node_2}
  end

  def seed_nodes() do
    # node 1
    cfg1 = node_config()
    node_1 = ENode.start_node(node_config: cfg1, node_id: cfg1.node_id)

    # create an advert for node 1 to store in the seed list of node 2
    address = %GRPCAddress{host: cfg1.grpc_host, port: cfg1.grpc_port}

    node2_info = %Advert{
      node_id: cfg1.node_id,
      grpc_address: address,
      version: "unknown"
    }

    # node 2
    cfg2 =
      node_config()
      |> Map.put(:seed_nodes, %{cfg1.node_id => node2_info})

    node_2 = ENode.start_node(node_id: cfg2.node_id, node_config: cfg2)

    # assert that node 1's network register know node 2.
    node_1_register = NetworkRegister.dump_register(cfg1.node_id)
    assert Map.get(node_1_register.nodes, cfg2.node_id) != nil

    # return the nodes
    {node_1, node_2}
  end
end
