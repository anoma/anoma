defmodule Anoma.Node.Transport.TCPConnection do
  @moduledoc """
  I manage an individual TCP connection and stream.
  I can run in two modes: either I can act as a client, initiating a connection
  to a server, or as a listener, waiting to accept and then accepting a
  connection on a server.
  In the latter case, since accepting the connection marks me as its owner and
  handoff is complicated, I start up a new listener once I accept a connection.
  """
  alias Anoma.Node.Router
  alias Anoma.Node.Transport
  alias __MODULE__

  require Logger
  use Transport.Connection
  use TypedStruct

  typedstruct do
    field(:router, Router.addr())
    field(:transport, Router.addr())
    field(:connection_pool, Supervisor.supervisor())
    field(:mode, :client | :listener)
    # only if listener
    field(:listener, reference() | nil)
    field(:conn, reference() | nil)
  end

  # TODO: annoyingly, we can't initiate tcp connections asynchronously, so it's
  # not clear how to cleanly abort the connection attempt
  # still, we can initiate the connection in a continue, so we don't block
  # whoever started us
  def init({:client, router, transport, address, connection_pool}) do
    {:ok,
     %TCPConnection{
       router: router,
       transport: transport,
       connection_pool: connection_pool,
       mode: :client
     }, {:continue, {:init_connection, address}}}
  end

  # ditto for listening
  def init({:listener, router, transport, listener, connection_pool}) do
    {:ok,
     %TCPConnection{
       router: router,
       transport: transport,
       connection_pool: connection_pool,
       mode: :listener,
       listener: listener
     }, {:continue, :accept_connection}}
  end

  def handle_continue({:init_connection, address}, s) do
    res =
      case address do
        {:unix, path} ->
          :gen_tcp.connect({:local, path}, 0, mode: :binary)

        {:tcp, host, port} ->
          :gen_tcp.connect(to_charlist(host), port, mode: :binary)
      end

    case res do
      {:ok, conn} ->
        Transport.new_connection(
          s.transport,
          Transport.transport_type(address)
        )

        {:noreply, %{s | conn: conn}}

      err ->
        die(s, inspect(err))
    end
  end

  def handle_continue(:accept_connection, s) do
    res = :gen_tcp.accept(s.listener)
    start_listener(s)

    case res do
      {:ok, conn} ->
        # need to figure out if unix or tcp
        Transport.new_connection(s.transport, :unix)
        {:noreply, %{s | conn: conn}}

      err ->
        die(s, inspect(err))
    end
  end

  def handle_cast({:send, msg}, _, s) do
    case :gen_tcp.send(s.conn, msg) do
      {:error, error} -> die(s, inspect(error))
      _ -> {:noreply, s}
    end
  end

  def handle_cast(:shutdown, _, s) do
    :gen_tcp.shutdown(s.conn, :read_write)
    {:noreply, s}
  end

  def handle_info({:tcp_closed, _}, s) do
    die(s, "connection shutdown")
  end

  def handle_info({:tcp, _, data}, s) do
    Transport.receive_chunk(s.transport, data)
    {:noreply, s}
  end

  defp die(s, reason) do
    Transport.disconnected(s.transport, reason)
    {:stop, :normal, s}
  end

  defp start_listener(s) do
    Router.start_engine(
      s.router,
      __MODULE__,
      {:listener, s.router, s.transport, s.listener, s.connection_pool},
      supervisor: s.connection_pool
    )
  end
end
