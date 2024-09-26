defmodule Anoma.Node.Examples.ETransport.ETcp do
  use Memoize

  import ExUnit.Assertions
  import Examples.ECrypto, only: [alice: 0, bertha: 0]

  alias Anoma.Crypto.Id
  alias Anoma.Node.Transport
  alias Anoma.Node.Transport.Registry
  alias Anoma.Node.Transport.Router
  alias Anoma.Node.Transport.TCP

  require ExUnit.Assertions
  require Logger

  ############################################################
  #                  Types                                   #
  ############################################################

  @typedoc """
  I am the type of a context.

  A context holds all the information about the started nodes and
  allows the components to control them.
  """
  @type context :: %{
          node_id: Id.t(),
          supervisor: pid(),
          ports: [integer()]
        }

  @typedoc """
  I am a map of contexts.

  Each context has name which can be used as a reference throughout the examples.
  """
  @type contexts :: %{atom() => context}

  ############################################################
  #                  Initialization                          #
  ############################################################

  @spec create_context(Id.t()) :: context
  def create_context(node_id) do
    # ensure the event broker is started
    Application.ensure_all_started(:event_broker)

    # start the transport supervisor with the given node id
    opts = [node_id: node_id]
    {:ok, supervisor_pid} = Transport.Supervisor.start_link(opts)

    # --------------------------------------------------------
    # Assert the processes are all created

    # assert that the registry is alive
    registry_name = Registry.registry_name(node_id)
    assert Process.alive?(Process.whereis(registry_name))

    # assert that the router is alive
    [{_node_id, Router, _, pid, _}] =
      Registry.lookup(node_id, node_id, Router)

    assert Process.alive?(pid)

    # assert that the tcp supervisor is alive
    [{_node_id, :tcp_supervisor, _, pid, _}] =
      Registry.lookup(node_id, node_id, :tcp_supervisor)

    assert Process.alive?(pid)

    # assert that the proxy engine supervisor is alive
    [{_node_id, :proxy_engine_supervisor, _, pid, _}] =
      Registry.lookup(node_id, node_id, :proxy_engine_supervisor)

    assert Process.alive?(pid)

    # --------------------------------------------------------
    # return the context

    %{node_id: node_id, supervisor: supervisor_pid, ports: []}
  end

  def cleanup(contexts, bob) do
    bob_node = Map.get(contexts, bob)
    Supervisor.stop(bob_node.supervisor)
    contexts
  end

  ############################################################
  #                           Components                     #
  ############################################################

  @doc """
  I create a new node for the alias Alice.
  """
  @spec create_node_alice() :: contexts
  @spec create_node_alice(contexts) :: contexts
  def create_node_alice(contexts \\ %{}) do
    Map.put(contexts, :alice, create_context(alice()))
  end

  @doc """
  I create a new node for the alias Bob.
  """
  @spec create_node_bob() :: contexts
  @spec create_node_bob(contexts) :: contexts
  def create_node_bob(contexts \\ %{}) do
    Map.put(contexts, :bob, create_context(bertha()))
  end

  @doc """
  I create a new tcp server for the node with the given name.
  """
  @spec create_tcp_server(contexts, for: atom()) :: contexts
  def create_tcp_server(contexts, for: bob) do
    bob_node = Map.get(contexts, bob)
    bob_id = bob_node.node_id

    # create a tcp server on a random port
    host = {0, 0, 0, 0}
    port = 0

    {:ok, port} = Router.start_tcp_server(host, port, node_id: bob_id)

    # assert the tcp server process exists
    [{_node_id, :tcp_listener, _, pid, nil}] =
      Registry.lookup(bob_id, bob_id, :tcp_listener)

    Process.alive?(pid)

    # store the port in the context
    bob_node = Map.update(bob_node, :ports, [port], &(&1 ++ [port]))

    # update the contexts
    Map.put(contexts, bob, bob_node)
  end

  @doc """
  I create a new tcp clietn for the node witht he given name in the context.
  I take the first available port from the node with the given server name.
  """
  @spec create_tcp_client(contexts, from: atom(), to: atom()) :: contexts
  def create_tcp_client(contexts, from: bob, to: alice) do
    bob = Map.get(contexts, bob)
    bob_id = bob.node_id

    alice = Map.get(contexts, alice)

    # get the first port from the server context
    port = List.first(alice.ports)

    host = {0, 0, 0, 0}
    Router.start_tcp_client(host, port, node_id: bob_id)

    contexts
  end

  @doc """
  I assert that the server has a connection process for talking to the client
  registered in its registry.
  """
  @spec assert_tcp_server_connection(contexts, from: atom(), to: atom()) ::
          contexts
  def assert_tcp_server_connection(contexts, from: alice, to: bob) do
    alice = Map.get(contexts, alice)
    alice_id = alice.node_id

    bob = Map.get(contexts, bob)
    bob_id = bob.node_id

    # alice should have a tcp server to bob
    [{^bob_id, TCP.Server, _, pid, _}] =
      Registry.lookup(alice_id, bob_id, TCP.Server)

    assert Process.alive?(pid)

    # bob should have a tcp client to alice
    [{^alice_id, TCP.Client, _, pid, _}] =
      Registry.lookup(bob_id, alice_id, TCP.Client)

    assert Process.alive?(pid)
    contexts
  end

  @doc """
  Given a context and two names, I verify that these two nodes
  have discovered eachother.
  """
  @spec assert_discovery(contexts, atom(), knows: atom()) :: contexts
  def assert_discovery(contexts, bob, knows: alice) do
    bob_node = Map.get(contexts, bob)
    bob_id = bob_node.node_id

    alice_node = Map.get(contexts, alice)
    alice_id = alice_node.node_id

    # verify that bob has a router proxy for alice
    assert Kernel.match?(
             [{_node_id, Router, _, _, _}],
             Registry.lookup(bob_id, alice_id, Router)
           )

    contexts
  end

  @doc """
  I send an asynchronous message from bob to alice's router.
  """
  @spec send_async_message(contexts, from: atom(), to: atom(), message: any()) ::
          contexts
  def send_async_message(contexts, from: bob, to: alice, message: m) do
    bob_node = Map.get(contexts, bob)
    bob_id = bob_node.node_id

    alice_node = Map.get(contexts, alice)
    alice_id = alice_node.node_id

    Router.send_async(alice_id, Router, m, node_id: bob_id)

    contexts
  end

  @doc """
  I send a synchronous message from bob to alice's router.
  """
  @spec send_sync_message(contexts, from: atom(), to: atom(), message: any()) ::
          contexts
  def send_sync_message(contexts, from: bob, to: alice, message: m) do
    bob_node = Map.get(contexts, bob)
    bob_id = bob_node.node_id

    alice_node = Map.get(contexts, alice)
    alice_id = alice_node.node_id

    Router.send_sync(alice_id, Router, m, node_id: bob_id, timeout: 10_000)

    contexts
  end

  @spec delay(contexts, integer()) :: contexts
  def delay(contexts, timeout \\ 500) do
    Process.sleep(timeout)
    contexts
  end

  ############################################################
  #                           Scenarios                      #
  ############################################################

  @doc """
  I create contexts with both bob and alice nodes.
  """
  @spec create_context() :: contexts
  def create_context() do
    create_node_alice()
    |> create_node_bob()
  end

  @doc """
  Given contexts, I destroy all of them.
  """
  @spec destroy_context(contexts) :: contexts
  def destroy_context(contexts) do
    contexts
    |> Enum.each(fn {bob, _} -> cleanup(contexts, bob) end)
    contexts
  end

  @doc """
  I creat two nodes, and connect them via tcp.
  I then check that both nodes know each other.
  """
  @spec establish_connection_over_tcp(contexts) :: contexts
  def establish_connection_over_tcp(contexts) do
    contexts
    |> create_tcp_server(for: :alice)
    |> create_tcp_client(from: :bob, to: :alice)
    |> delay()
    |> assert_tcp_server_connection(from: :alice, to: :bob)
    |> assert_discovery(:alice, knows: :bob)
    |> assert_discovery(:bob, knows: :alice)
  end

  @doc """
  I send an asynchronous message to bob.
  """
  @spec send_async_message_to_bob(contexts) :: contexts
  def send_async_message_to_bob(contexts) do
    contexts
    |> send_async_message(
      from: :alice,
      to: :bob,
      message: "hello from alice to bob"
    )
  end

  @doc """
  I send a synchronous message to bob.
  """
  @spec send_sync_message_to_bob(contexts) :: contexts
  def send_sync_message_to_bob(contexts) do
    contexts
    |> send_sync_message(
      from: :alice,
      to: :bob,
      message: "hello from alice to bob"
    )
  end

  @doc """
  I send a synchronous message to bob while they're offline.
  The message will be delivered when bob comes back.
  """
  @spec send_sync_while_offline_to_bob(contexts) :: contexts
  def send_sync_while_offline_to_bob(contexts) do
    contexts
    |> cleanup(:bob)
    |> tap(fn contexts ->
      spawn(fn ->
        send_sync_message(contexts,
          from: :alice,
          to: :bob,
          message: "sent when disconnected"
        )
      end)
    end)
    |> delay()
    |> create_node_bob()
    |> create_tcp_client(from: :bob, to: :alice)
    |> delay()
  end

  @doc """
  I send a synchronous message to bob while they're offline.
  The message will be delivered when bob comes back.
  """
  @spec send_async_while_offline_to_bob(contexts) :: contexts
  def send_async_while_offline_to_bob(contexts) do
    contexts
    |> cleanup(:bob)
    |> send_async_message(
      from: :alice,
      to: :bob,
      message: "sent when disconnected"
    )
    |> delay()
    |> create_node_bob()
    |> create_tcp_client(from: :bob, to: :alice)
    |> delay()
  end
end
