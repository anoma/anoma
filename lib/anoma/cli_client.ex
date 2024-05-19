defmodule Anoma.Cli.Client do
  @moduledoc """
  I am a small engine that runs in a CLI's node and connects to and talks to
  the local administration engine on another node running on the same system.

  I have to be a dedicated engine because of the protocol design: every message
  must originate in a partcular engine with its own id.  (Proposed specs
  changes may obviate this in the future.)
  """

  alias Anoma.Node.Router
  alias Anoma.Node.Transport
  use Router.Engine

  def init(operation) do
    {:ok, nil, {:continue, operation}}
  end

  @spec handle_continue({Router.addr(), Router.addr(), any()}, any()) ::
          no_return()
  def handle_continue({router, transport, operation}, _) do
    # load info of the running node, erroring if it appears not to exist, and
    # attempt to introduce ourselves to it
    # there should be a better way to find out its id(s)
    dump_path = Anoma.System.Directories.data("node_keys.dmp")
    sock_path = Anoma.System.Directories.data("local.sock")

    if not File.exists?(sock_path) do
      IO.puts("Local node configuration socket #{sock_path} not found")
      System.halt(1)
    end

    primary = Anoma.Dump.load(dump_path)

    # tell our transport how to reach the node
    Transport.learn_node(
      transport,
      primary.router.external,
      {:unix, sock_path}
    )

    # and how to reach its transport engine and mempool and storage
    Transport.learn_engine(
      transport,
      primary.transport_id,
      primary.router.external
    )

    Transport.learn_engine(
      transport,
      elem(primary.mempool, 0),
      primary.router.external
    )

    Transport.learn_engine(
      transport,
      elem(primary.ordering, 0),
      primary.router.external
    )

    Transport.learn_engine(
      transport,
      primary.router.external,
      primary.router.external
    )

    # form an address.  this should be abstracted properly
    other_transport_addr = %{
      router
      | server: nil,
        id: primary.transport_id
    }

    # tell the other router how to reach us
    Transport.learn_engine(
      other_transport_addr,
      Router.self_addr(router).id,
      router.id
    )

    # ensure we have a connection (there should be a better way to do
    # this--connection establishment should have its own timeouts built-in, and
    # we should get notified when connection establishment succeeds/fails)
    # use a shorter timeout because--come on
    with {:error, :timed_out} <-
           Router.call(other_transport_addr, :ping, 1000) do
      IO.puts("Unable to connect to local node")
      System.halt(1)
    end

    perform(operation, primary, router)

    # synchronise--make sure the queues get flushed properly before we exit
    Router.call(other_transport_addr, :ping)

    System.halt(0)
  end

  defp perform({:submit_tx, path}, primary, router) do
    other_mempool_addr = %{
      router
      | server: nil,
        id: elem(primary.mempool, 0)
    }

    tx =
      case File.read(path) do
        {:ok, tx} ->
          tx

        {:error, error} ->
          IO.puts(
            "Failed to load transaction from file #{path}: #{inspect(error)}"
          )

          System.halt(1)
      end

    case Noun.Format.parse(tx) do
      {:ok, tx} ->
        Anoma.Node.Mempool.async_tx(other_mempool_addr, {:kv, tx})

      :error ->
        IO.puts("Failed to parse transaction from file #{path}")

        System.halt(1)
    end
  end

  defp perform(:shutdown, primary, router) do
    other_router_addr = %{
      router
      | server: nil,
        id: primary.router.external
    }

    Anoma.Node.Router.shutdown_node(other_router_addr)
  end

  defp perform({:get_key, key}, primary, router) do
    other_ordering_addr = %{
      router
      | server: nil,
        id: elem(primary.ordering, 0)
    }

    case Anoma.Node.Ordering.get(other_ordering_addr, key) do
      {:error, :timed_out} ->
        IO.puts("Connection error")
        System.halt(1)

      :absent ->
        IO.puts("no such key")

      {:ok, value} ->
        IO.puts(inspect(value))
    end
  end
end
