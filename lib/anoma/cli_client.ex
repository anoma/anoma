defmodule Anoma.Cli.Client do
  @moduledoc """
  I am a small engine that runs in a CLI's node and connects to and talks to
  the local administration engine on another node running on the same system.

  I have to be a dedicated engine because of the protocol design: every message
  must originate in a particular engine with its own id. (Proposed specs
  changes may obviate this in the future.)
  """

  alias Anoma.RM.Resource
  alias Anoma.RM.Transparent.Transaction
  alias Anoma.Dump
  alias Anoma.Node.Router
  alias Anoma.Node.Transport
  alias Anoma.Node.Router.Addr
  use Router.Engine

  def init(operation) do
    {:ok, nil, {:continue, operation}}
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @doc """
  I get the error code from the ran client function
  """
  @spec error_code(Router.Addr.t()) :: integer() | nil
  def error_code(server) do
    Router.call(server, :error_code)
  end

  @doc """
  I get the error code from the ran client function
  """
  @spec return_value(Router.Addr.t()) :: any()
  def return_value(server) do
    Router.call(server, :return_value)
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_call(:error_code, _, state = {return_code, _}) do
    {:reply, return_code, state}
  end

  def handle_call(:return_value, _, state = {_, return_value}) do
    {:reply, return_value, state}
  end

  @spec handle_continue(
          {Router.addr(), Router.addr(), Anoma.Node.t() | Dump.dump(),
           Transport.transport_addr(), any(), boolean()},
          any()
        ) ::
          {:noreply, {0 | 1, any()}}
  def handle_continue(
        {router, transport, server, sock_addr, operation, silent},
        _
      ) do
    if not File.exists?(elem(sock_addr, 1)) do
      {:noreply, try_offline(operation)}
    else
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
      with :pong <- Router.call(server_engines.transport, :ping, 1000) do
        {:ok, output} = StringIO.open("")

        results = perform(operation, server_engines, output)

        # synchronise--make sure the queues get flushed properly before we exit
        Router.call(server_engines.transport, :ping)

        unless silent do
          StringIO.flush(output) |> IO.puts()
        end

        StringIO.close(output)

        {:noreply, results}
      else
        _ ->
          {:noreply, try_offline(operation)}
      end
    end
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  @spec try_offline(any()) :: {0 | 1, any()}
  defp try_offline(operation) do
    IO.puts("Unable to connect to local node")
    IO.puts("Trying offline commands")
    perform_offline(operation)
  end

  defp perform({:submit_tx, path}, server_engines, output) do
    do_submit(path, server_engines, :kv, output)
  end

  defp perform({:rm_submit_tx, path}, server_engines, output) do
    do_submit(path, server_engines, :rm, output)
  end

  defp perform({:ro_submit_tx, path}, server_engines, output) do
    client_addr = Router.Addr.id(Router.self_addr())

    case do_submit(path, server_engines, :ro, output, client_addr) do
      {0, _} ->
        receive do
          {:"$gen_cast", {:router_external_cast, _, payload}} ->
            case Anoma.Serialise.unpack(payload) do
              {:ok, {:read_value, value}} ->
                IO.puts(output, inspect(value))
                {0, value}

              {:ok, {:worker_error}} ->
                IO.puts(output, "Execution error")
                {0, nil}

              _ ->
                IO.puts(output, "Unexpected response")
                {0, nil}
            end
        end

      res ->
        res
    end
  end

  defp perform({:check_commitment, comm}, server_engines, output) do
    ["rm", "commitments"]
    |> base64_storage_check(comm, server_engines, output)
  end

  defp perform({:check_nulifier, null}, server_engines, output) do
    ["rm", "nullifiers"]
    |> base64_storage_check(null, server_engines, output)
  end

  defp perform(:shutdown, server_engines, _output) do
    Anoma.Node.Router.shutdown_node(server_engines.router)
    {0, nil}
  end

  defp perform({:get_key, key}, server_engines, output) do
    case Anoma.Node.Storage.get(server_engines.storage, key) do
      {:error, :timed_out} ->
        IO.puts("Connection error")
        {1, :timed_out}

      :absent ->
        IO.puts(output, "no such key")
        {0, :absent}

      {:ok, value} ->
        IO.puts(output, inspect(value))
        {0, value}
    end
  end

  defp perform(:snapshot, server_engines, output) do
    Anoma.Node.Configuration.snapshot(server_engines.configuration)

    receive do
      {:"$gen_cast", {:router_external_cast, _, payload}} ->
        case Anoma.Serialise.unpack(payload) do
          {:ok, :snapshot_done} ->
            IO.puts(output, "Snapshot done")

          _ ->
            IO.puts(output, "Unexpected response")
        end
    end

    {0, nil}
  end

  defp perform(:delete_dump, server_engines, _output) do
    Anoma.Node.Configuration.delete_dump(server_engines.configuration)
    {0, nil}
  end

  defp perform(_, _, _) do
    {1, nil}
  end

  @spec perform_offline(any()) :: {0 | 1, any()}
  defp perform_offline(:delete_dump) do
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
      {0, nil}
    else
      IO.puts(
        "Can not find Dump file, please delete the dumped data yourself"
      )

      {1, nil}
    end
  end

  defp perform_offline(_) do
    {1, nil}
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

  defp base64_storage_check(
         path_prefix,
         base64_path_suffix,
         server_engines,
         output
       ) do
    case Base.decode64(base64_path_suffix) do
      {:ok, decoded_suffix} ->
        full_path = path_prefix ++ [decoded_suffix]
        perform({:get_key, full_path}, server_engines, output)

      :error ->
        IO.puts("Error decoding key")
        1
    end
  end

  defp do_submit(path, server_engines, kind, output, reply_to \\ nil) do
    with {:ok, tx} <- File.read(path),
         {:ok, tx} <- Noun.Format.parse(tx) do
      jammed = Nock.Jam.jam(tx)

      case kind do
        :rm ->
          # We don't want to be proving online so this shouldn't be
          # scrying. We should design something better for V2
          {:ok, tx} = Nock.nock(tx, [9, 2, 0 | 1])
          tx_proper = Transaction.from_noun(tx)

          for commitment <- tx_proper.commitments do
            IO.puts(
              output,
              "commitment: #{Resource.commitment_hash(commitment) |> Base.encode64()}"
            )
          end

          for nullifier <- tx_proper.nullifiers do
            IO.puts(
              output,
              "nullifier: #{Resource.commitment_hash(nullifier) |> Base.encode64()}"
            )
          end

        _ ->
          nil
      end

      Anoma.Node.Mempool.tx(server_engines.mempool, {kind, jammed}, reply_to)
      {0, nil}
    else
      {:error, error} ->
        IO.puts(
          "Failed to load transaction from file #{path}: #{inspect(error)}"
        )

        {1, nil}

      :error ->
        IO.puts("Failed to parse transaction from file #{path}")
        {1, nil}
    end
  end
end
