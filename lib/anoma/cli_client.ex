defmodule Anoma.Cli.Client do
  @moduledoc """
  I am a small engine that runs in a CLI's node and connects to and talks to
  the local administration engine on another node running on the same system.

  I have to be a dedicated engine because of the protocol design: every message
  must originate in a partcular engine with its own id.  (Proposed specs
  changes may obviate this in the future.)
  """

  alias Anoma.Dump
  alias Anoma.Node.Router
  alias Anoma.Node.Transport
  alias Anoma.Node.Router.Addr
  use Router.Engine

  def init(operation) do
    {:ok, nil, {:continue, operation}}
  end

  @spec handle_continue(
          {Router.addr(), Router.addr(), Anoma.Node.t() | Dump.dump(),
           Transport.transport_addr(), any()},
          any()
        ) ::
          no_return()
  def handle_continue({router, transport, server, sock_addr, operation}, _) do
    if not File.exists?(elem(sock_addr, 1)) do
      IO.puts("Local node configuration socket #{sock_addr} not found")
      IO.puts("Unable to connect to local node")
      IO.puts("Trying offline commands")
      perform_offline(operation)
    end

    learn_about_the_server(server, transport, sock_addr)

    server_engines = server_engines(server, router)

    # tell the other router how to reach us
    Transport.learn_engine(
      server_engines.transport,
      Router.Addr.id(Router.self_addr()),
      router.id
    )

    # ensure we have a connection (there should be a better way to do
    # this--connection establishment should have its own timeouts built-in, and
    # we should get notified when connection establishment succeeds/fails)
    # use a shorter timeout because--come on
    with {:error, :timed_out} <-
           Router.call(server_engines.transport, :ping, 1000) do
      IO.puts("Unable to connect to local node")
      IO.puts("Trying offline commands")
      perform_offline(operation)
      System.halt(1)
    end

    perform(operation, server_engines)

    # synchronise--make sure the queues get flushed properly before we exit
    Router.call(server_engines.transport, :ping)

    System.halt(0)
  end

  defp perform({:submit_tx, path}, server_engines) do
    do_submit(path, server_engines, :kv)
  end

  defp perform({:rm_submit_tx, path}, server_engines) do
    do_submit(path, server_engines, :rm)
  end

  defp perform(:shutdown, server_engines) do
    Anoma.Node.Router.shutdown_node(server_engines.router)
  end

  defp perform({:get_key, key}, server_engines) do
    case Anoma.Node.Storage.get(server_engines.storage, key) do
      {:error, :timed_out} ->
        IO.puts("Connection error")
        System.halt(1)

      :absent ->
        IO.puts("no such key")

      {:ok, value} ->
        IO.puts(inspect(value))
    end
  end

  defp perform(:snapshot, server_engines) do
    Anoma.Node.Configuration.snapshot(server_engines.configuration)
  end

  defp perform(:delete_dump, server_engines) do
    Anoma.Node.Configuration.delete_dump(server_engines.configuration)
  end

  def perform_offline(:delete_dump) do
    # Assume server is running prod
    config = Anoma.Configuration.default_configuration_location(:prod)

    dump_file =
      if File.exists?(config) do
        config
        |> Anoma.Configuration.read_configuration()
        |> Anoma.Configuration.locate_dump_file()
      else
        Anoma.Configuration.default_data_location(:prod)
      end

    if dump_file && File.exists?(dump_file) do
      IO.puts("Deleting dump file: #{dump_file}")
      File.rm!(dump_file)
    else
      IO.puts(
        "Can not find Dump file, please delete the dumped data yourself"
      )
    end
  end

  ############################################################
  #                    Server Abstractions                   #
  ############################################################

  @spec learn_about_the_server(
          Anoma.Node.t() | Dump.dump(),
          Router.addr(),
          Transport.transport_addr()
        ) ::
          any()
  defp learn_about_the_server(server = %Anoma.Node{}, transport, sock_path) do
    # tell our transport how to reach the node
    Transport.learn_node(
      transport,
      server.router |> Addr.id(),
      sock_path
    )

    # and how to reach its transport engine and mempool and storage
    [
      server.transport,
      server.mempool,
      server.storage,
      server.configuration,
      server.router
    ]
    |> Enum.map(&Addr.id/1)
    |> learn_list(transport, server.router |> Addr.id())
  end

  defp learn_about_the_server(server = %{}, transport, sock_path) do
    # tell our transport how to reach the node
    Transport.learn_node(
      transport,
      server.router.external,
      sock_path
    )

    # and how to reach its transport engine and mempool and storage
    [
      server.transport_id,
      elem(server.mempool, 0),
      elem(server.storage, 0),
      elem(server.configuration, 0),
      server.router.external
    ]
    |> learn_list(transport, server.router.external)
  end

  defp learn_list(xs, transport, router_id) when is_list(xs) do
    xs
    |> Enum.each(&Transport.learn_engine(transport, &1, router_id))
  end

  @spec server_engines(Anoma.Node.t() | Dump.dump(), Router.addr()) :: %{
          atom() => Router.addr()
        }
  defp server_engines(server = %Anoma.Node{}, our_r) do
    %{
      transport: %{our_r | server: nil, id: server.transport |> Addr.id()},
      router: %{our_r | server: nil, id: server.router |> Addr.id()},
      storage: %{our_r | server: nil, id: server.storage |> Addr.id()},
      configuration: %{
        our_r
        | server: nil,
          id: server.configuration |> Addr.id()
      },
      mempool: %{our_r | server: nil, id: server.mempool |> Addr.id()}
    }
  end

  defp server_engines(server = %{}, our_r) do
    # form an address.  this should be abstracted properly
    %{
      transport: %{our_r | server: nil, id: server.transport_id},
      router: %{our_r | server: nil, id: server.router.external},
      storage: %{our_r | server: nil, id: elem(server.storage, 0)},
      configuration: %{our_r | server: nil, id: elem(server.configuration, 0)},
      mempool: %{our_r | server: nil, id: elem(server.mempool, 0)}
    }
  end

  ############################################################
  #                        Helper                            #
  ############################################################

  defp do_submit(path, server_engines, kind) do
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
        Anoma.Node.Mempool.tx(server_engines.mempool, {kind, tx})

      :error ->
        IO.puts("Failed to parse transaction from file #{path}")

        System.halt(1)
    end
  end
end
