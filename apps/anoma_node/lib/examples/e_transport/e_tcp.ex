defmodule Anoma.Node.Examples.ETransport.ETcp do
  use Memoize
  use TypedStruct

  import ExUnit.Assertions
  import Examples.ECrypto, only: [alice: 0, bertha: 0]

  alias Anoma.Crypto.Id
  alias Anoma.Node.Registry
  alias Protobufs.NodeInfo
  alias EventBroker.Filters
  alias Anoma.Node.Examples.ENode

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

  # @doc """
  # I create a new node and add it to the context.
  # """
  @spec create_node(contexts, atom(), Id.t()) :: contexts
  defp create_node(contexts, node_name, node_id) do
    # start  a node for alice
    node_pid = ENode.start_node(node_id)

    # --------------------------------------------------------
    # Assert the processes are all created

    registered_processes =
      Registry.dump_register()
      |> Enum.map(fn {address, _pid, _} -> address end)

    assert Registry.address(node_id, :tcp_supervisor) in registered_processes

    assert Registry.address(node_id, :proxy_supervisor) in registered_processes

    # --------------------------------------------------------
    # return the context

    Map.put(contexts, node_name, %Context{node_id: node_id, pid: node_pid})
  end

  # @doc """
  # I remove the node with the given name from the context.
  # """
  @spec remove_node(contexts, atom()) :: contexts
  def remove_node(contexts, node_name) do
    %{node_id: node_id, pid: node_pid} = Map.get(contexts, node_name)

    # get the pids of the processes that are expected to be gone.
    # we need to check that they are gone after calling stop.
    #
    # The reason for this is that the registry is not immediately updated
    # and can still contain the old pids.
    tcp_supervisor_pid = Registry.whereis(node_id, :tcp_supervisor)
    proxy_supervisor_pid = Registry.whereis(node_id, :proxy_supervisor)

    Supervisor.stop(node_pid, :normal)

    # --------------------------------------------------------
    # Assert the processes are all stoppped

    refute Process.alive?(tcp_supervisor_pid)
    refute Process.alive?(proxy_supervisor_pid)

    # --------------------------------------------------------
    # return the context

    contexts
    |> Map.delete(node_name)
  end

  ############################################################
  #                           Components                     #
  ############################################################

  @doc """
  I create a context with two default nodes.
  """
  @spec create_context() :: contexts
  def create_context() do
    %{}
    |> create_node(:alice, alice())
    |> create_node(:bertha, bertha())
  end

  # @doc """
  # I create a tcp server for the given node.
  # """
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
    Map.update(node, :ports, [port], &(&1 ++ [port]))
  end

  # @doc """
  # I create a tcp server for the given node.
  # """
  @spec create_tcp_listener(contexts, for: atom()) :: contexts
  def create_tcp_listener(contexts, for: node_name) do
    node = Map.get(contexts, node_name)
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
    node = Map.update(node, :ports, [port], &(&1 ++ [port]))

    # update the contexts
    Map.put(contexts, node_name, node)
  end

  # @doc """
  # I create a tcp client for the given node and connect to the server node.
  # """
  @spec create_tcp_client(contexts, for: atom(), to: atom()) :: contexts
  def create_tcp_client(contexts, for: node_name, to: server_name) do
    node = Map.get(contexts, node_name)
    node_id = node.node_id

    server = Map.get(contexts, server_name)
    server_node_id = server.node_id

    server_port = Map.get(contexts, server_name).ports |> List.first()

    # count the number of client connections before creating a new one
    clients_before =
      DynamicSupervisor.count_children(
        Registry.whereis(node_id, :tcp_supervisor)
      )

    # count the number of connections on the server before creating the connection
    server_before =
      DynamicSupervisor.count_children(
        Registry.whereis(server_node_id, :tcp_supervisor)
      )

    # start a tcp server on a random port
    {:ok, _pid, port} =
      Anoma.Node.Transport.start_tcp_client(
        node_id,
        {{0, 0, 0, 0}, server_port}
      )

    clients_after =
      DynamicSupervisor.count_children(
        Registry.whereis(node_id, :tcp_supervisor)
      )

    server_after =
      DynamicSupervisor.count_children(
        Registry.whereis(server_node_id, :tcp_supervisor)
      )

    # assert that one extra client connection is created
    assert clients_before.active + 1 == clients_after.active
    assert clients_before.workers + 1 == clients_after.workers
    assert clients_before.specs + 1 == clients_after.specs

    # assert that the server has one extra connection
    assert server_before.active + 1 == server_after.active
    assert server_before.workers + 1 == server_after.workers
    assert server_before.specs + 1 == server_after.specs

    # store the port in the context
    node = Map.update(node, :ports, [port], &(&1 ++ [port]))

    # update the contexts
    Map.put(contexts, node_name, node)
  end

  ############################################################
  #                           Scenarios                      #
  ############################################################

  @doc """
  I create a context, and then tear it down again.

  I check that the nodes are all properly cleaned up.
  """
  @spec create_nodes_and_cleanup_nodes() :: contexts
  def create_nodes_and_cleanup_nodes() do
    %{}
    |> create_node(:alice, alice())
    |> create_node(:bertha, bertha())
    |> remove_node(:alice)
    |> remove_node(:bertha)
  end

  @doc """
  I create a new tcp server for the node with the given name.
  """
  @spec connect_nodes() :: contexts
  def connect_nodes() do
    # subscribe to discovery events
    EventBroker.subscribe_me([
      %Filters.SourceModule{module: Anoma.Node.Transport.TCP.Connection}
    ])

    # create the nodes and connect them to each other.
    contexts =
      %{}
      |> create_node(:alice, alice())
      |> create_node(:bertha, bertha())
      |> create_tcp_listener(for: :alice)
      |> create_tcp_client(for: :bertha, to: :alice)

    # the node info we expect to discover
    alice_node_info = %NodeInfo{
      sign: alice().external.sign,
      encrypt: alice().external.encrypt
    }

    assert_receive %{body: {:new_node_discovered, ^alice_node_info}}

    # the node info we expect to discover
    bertha_node_info = %NodeInfo{
      sign: bertha().external.sign,
      encrypt: bertha().external.encrypt
    }

    assert_receive %{body: {:new_node_discovered, ^bertha_node_info}}

    contexts
  end
end
