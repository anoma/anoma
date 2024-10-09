defmodule Anoma.Node.Examples.ETransport.ETcp do
  use Memoize
  use TypedStruct

  import ExUnit.Assertions

  alias Anoma.Crypto.Id
  alias Anoma.Node.Examples.ENode
  alias Anoma.Node.Registry

  require ExUnit.Assertions
  require Logger

  ############################################################
  #                    Context                               #
  ############################################################

  typedstruct module: Context do
    @typedoc """
    I am the state of a TCP listener.

    My fields contain information to listen for TCP connection with a remote node.

    ### Fields
    - `:node_id`  - The key of this router. This value is used to announce myself to other
    - `:pid`      - the pid of the supervision tree.
    - `:ports`    - The ports on which the node is listening for connections.
    """
    field(:node_id, Id.t())
    field(:pid, pid())
    field(:ports, [integer()], default: [])
  end

  @typedoc """
  I am the type of the contexts in this module.
  """
  @type contexts :: %{atom() => Context.t()}

  ############################################################
  #                  Initialization                          #
  ############################################################

  ############################################################
  #                           Components                     #
  ############################################################

  @doc """
  I create a tcp server for the given node.
  """
  @spec create_tcp_listener(ENode.t()) :: ENode.t()
  def create_tcp_listener(node) do
    node_id = node.node_id

    # count the number of listeners in use before creating a new one
    count_before =
      DynamicSupervisor.count_children(
        Registry.whereis(node_id, :tcp_supervisor)
      )

    # start a tcp server on a random port
    {:ok, _pid, port} = Anoma.Node.Transport.start_tcp_server(node_id)

    count_after =
      DynamicSupervisor.count_children(
        Registry.whereis(node_id, :tcp_supervisor)
      )

    # assert that one extra listener is created
    assert count_before.active + 1 == count_after.active
    assert count_before.workers + 1 == count_after.workers
    assert count_before.specs + 1 == count_after.specs

    # store the port in the context
    Map.update(node, :tcp_ports, [port], &(&1 ++ [port]))
  end

  @doc """
  I create a tcp client for the given node and connect to the server node.
  """
  @spec create_tcp_client(client: ENode.t(), server: ENode.t()) :: [
          client: ENode.t(),
          server: ENode.t()
        ]
  def create_tcp_client(client: client, server: server) do
    assert server.tcp_ports != []
    server_port = hd(server.tcp_ports)

    # count the number of client connections before creating a new one
    clients_before =
      DynamicSupervisor.count_children(
        Registry.whereis(client.node_id, :tcp_supervisor)
      )

    # count the number of connections on the server before creating the connection
    server_before =
      DynamicSupervisor.count_children(
        Registry.whereis(server.node_id, :tcp_supervisor)
      )

    # start a tcp server on a random port
    {:ok, _pid, _port} =
      Anoma.Node.Transport.start_tcp_client(
        client.node_id,
        {{0, 0, 0, 0}, server_port}
      )

    clients_after =
      DynamicSupervisor.count_children(
        Registry.whereis(client.node_id, :tcp_supervisor)
      )

    server_after =
      DynamicSupervisor.count_children(
        Registry.whereis(server.node_id, :tcp_supervisor)
      )

    # assert that one extra client connection is created
    assert clients_before.active + 1 == clients_after.active
    assert clients_before.workers + 1 == clients_after.workers
    assert clients_before.specs + 1 == clients_after.specs

    # assert that the server has one extra connection
    assert server_before.active + 1 == server_after.active
    assert server_before.workers + 1 == server_after.workers
    assert server_before.specs + 1 == server_after.specs

    [client: client, server: server]
  end

  ############################################################
  #                           Scenarios                      #
  ############################################################

  @doc """
  I create a new tcp server for the node with the given name.
  """
  @spec connect_nodes() :: {ENode.t(), ENode.t()}
  def connect_nodes() do
    server = ENode.start_node(Examples.ECrypto.alice())

    client = ENode.start_node(Examples.ECrypto.londo())

    connect_nodes({client, server})
  end

  @doc """
  I create a tcp connection between two nodes and assert that they discover eachother.
  """
  @spec connect_nodes({ENode.t(), ENode.t()}) :: {ENode.t(), ENode.t()}
  def connect_nodes({client, server}) do
    # subscribe to discovery events before connecting the nodes.
    EventBroker.subscribe_me([])

    # setup the tcp server in the node.
    server = create_tcp_listener(server)

    [client: client, server: server] =
      create_tcp_client(client: client, server: server)

    assert_receive %{body: {:new_node_discovered, _}}

    assert_receive %{body: {:new_node_discovered, _}}

    {client, server}
  end
end
