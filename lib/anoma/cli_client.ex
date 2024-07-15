defmodule Anoma.Cli.Client do
  @moduledoc """
  I am a small engine that runs in a CLI's node and connects to and talks to
  the local administration engine on another node running on the same system.

  I have to be a dedicated engine because of the protocol design: every message
  must originate in a partcular engine with its own id.  (Proposed specs
  changes may obviate this in the future.)
  """

  alias Anoma.Resource
  alias Anoma.Resource.Transaction
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
      elem(primary.storage, 0),
      primary.router.external
    )

    Transport.learn_engine(
      transport,
      elem(primary.configuration, 0),
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
      Router.Addr.id(Router.self_addr()),
      router.id
    )

    # ensure we have a connection (there should be a better way to do
    # this--connection establishment should have its own timeouts built-in, and
    # we should get notified when connection establishment succeeds/fails)
    # use a shorter timeout because--come on
    with {:error, :timed_out} <-
           Router.call(other_transport_addr, :ping, 1000) do
      IO.puts("Unable to connect to local node")
      IO.puts("Trying offline commands")
      perform_offline(operation)
      System.halt(1)
    end

    perform(operation, primary, router)

    # synchronise--make sure the queues get flushed properly before we exit
    Router.call(other_transport_addr, :ping)

    System.halt(0)
  end

  defp perform({:submit_tx, path}, primary, router) do
    do_submit(path, primary, router, :kv)
  end

  defp perform({:rm_submit_tx, path}, primary, router) do
    do_submit(path, primary, router, :rm)
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
    other_storage_addr = %{
      router
      | server: nil,
        id: elem(primary.storage, 0)
    }

    case Anoma.Node.Storage.get(other_storage_addr, key) do
      {:error, :timed_out} ->
        IO.puts("Connection error")
        System.halt(1)

      :absent ->
        IO.puts("no such key")

      {:ok, value} ->
        IO.puts(inspect(value))
    end
  end

  defp perform(:snapshot, primary, router) do
    other_configuration_addr = %{
      router
      | server: nil,
        id: elem(primary.configuration, 0)
    }

    Anoma.Node.Configuration.snapshot(other_configuration_addr)
  end

  defp perform(:delete_dump, primary, router) do
    other_configuration_addr = %{
      router
      | server: nil,
        id: elem(primary.configuration, 0)
    }

    Anoma.Node.Configuration.delete_dump(other_configuration_addr)
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
  #                        Helper                            #
  ############################################################

  defp do_submit(path, primary, router, kind) do
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
        jammed = Nock.Jam.jam(tx)

        case kind do
          :rm ->
            # We don't want to be proving online so this shouldn't be
            # scrying. We should design something better for V2
            {:ok, tx} = Nock.nock(tx, [9, 2, 0 | 1])
            tx_proper = Transaction.from_noun(tx)

            for commitment <- tx_proper.commitments do
              IO.puts(
                "commitment: #{Resource.commitment_hash(commitment) |> Base.encode64()}"
              )
            end

            for nullifier <- tx_proper.nullifiers do
              IO.puts(
                "nullifier: #{Resource.commitment_hash(nullifier) |> Base.encode64()}"
              )
            end

            Anoma.Node.Mempool.tx(other_mempool_addr, {kind, jammed})

          _ ->
            Anoma.Node.Mempool.tx(other_mempool_addr, {kind, jammed})
        end

      :error ->
        IO.puts("Failed to parse transaction from file #{path}")

        System.halt(1)
    end
  end
end
