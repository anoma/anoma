defmodule AnomaTest.Node.Transport do

  use ExUnit.Case, async: true

  alias Anoma.Storage
  alias Anoma.Node.Router
  alias Anoma.Node.Transport
  import TestHelper.Nock

  setup_all do
    storage = %Anoma.Storage{
      qualified: AnomaTest.Transport.Qualified,
      order: AnomaTest.Transport.Order
    }

    name = :transport
    snapshot_path = [:my_special_nock_snaphsot | 0]

    {:ok, nodes} =
      Anoma.Node.start_link(
        new_storage: true,
        name: name,
        settings:
          [
            snapshot_path: snapshot_path,
            storage: storage,
            block_storage: :mempool_blocks,
            ping_time: :no_timer
          ]
          |> Anoma.Node.start_min()
      )

    node = Anoma.Node.state(nodes)

    [node: node]
  end

  test "foo with logger", %{node: node} do
    port = 9864
    {:ok, hostname} = :inet.gethostname
    # this node ignores the hostname
    Transport.start_server(node.transport, {:tcp, hostname, port})

    name_other = :transport_other

    {:ok, nodes_o} =
      Anoma.Node.start_link(
        new_storage: true,
        name: name_other,
        settings:
          [
            snapshot_path: snapshot_path,
            storage: storage,
            block_storage: :mempool_blocks,
            ping_time: :no_timer
          ]
          |> Anoma.Node.start_min()
      )
    node_other = Anoma.Node.state(nodes_o)

    {:ok, router, transport} = {:ok, node_other.router, node_other.transport}

    # have it learn about the node, the hostname is important
    Transport.learn_node(transport, node.router.id, {:tcp, hostname, port})
    Transport.learn_engine(transport, node.transport.id, node.router.id)

    # let us learn about the mempool
    Transport.learn_engine(transport, node.mempool.id, node.router.id)

    # use our router to send messages, to their id
    transport_addr = %{router | server: nil, id: node.transport.id}

    # We set these so the router call isn't nil
    # We should maybe generate our own, rather than masquerading as the transport engine

    Process.put(:engine_id, transport.id)
    Process.put(:engine_server, transport.server)

    Logger.configure(level: :debug)

    Router.call(transport_addr, :ping, :infinity)
  end

  test "foo", %{node: node} do
    {:ok, hostname} = :inet.gethostname

    port = 9865

    # this node ignores the hostname
    Transport.start_server(node.transport, {:tcp, hostname, port})
    # Other transport and router
    {:ok, router, transport} = Anoma.Node.Router.start()

    # have it learn about the node, the hostname is important
    Transport.learn_node(transport, node.router.id, {:tcp, hostname, port})
    Transport.learn_engine(transport, node.transport.id, node.router.id)

    # let us learn about the mempool
    Transport.learn_engine(transport, node.mempool.id, node.router.id)

    # use our router to send messages, to their id
    transport_addr = %{router | server: nil, id: node.transport.id}

    # We set these so the router call isn't nil
    # We should maybe generate our own, rather than masquerading as the transport engine

    Process.put(:engine_id, transport.id)
    Process.put(:engine_server, transport.server)

    Logger.configure(level: :debug)

    Router.call(transport_addr, :ping, :infinity)
  end

end
