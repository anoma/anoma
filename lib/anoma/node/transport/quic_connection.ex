defmodule Anoma.Node.Transport.QuicConnection do
  @moduledoc """
  I manage an individual QUIC connection and stream.
  I can run in two modes: either I can act as a client, initiating a connection
  to a server, or as a listener, waiting to accept and then accepting a
  connection on a server.
  In the latter case, since accepting the connection marks me as its owner and
  handoff is complicated, I start up a new listener once I accept a connection.
  """
  alias Anoma.Node.Router
  alias Anoma.Node.Transport
  alias __MODULE__

  use Transport.Connection
  use TypedStruct

  typedstruct do
    field(:router, Router.addr())
    field(:transport, Router.addr())
    field(:mode, :client | :listener)
    # only if listener
    field(:listener, reference() | nil)
    field(:conn, reference() | nil)
    field(:stream, reference() | nil)
  end

  # initiate outgoing connection
  def init({:client, router, transport, {:quic, host, port}}) do
    with {:ok, conn} <-
           :quicer.async_connect(to_charlist(host), port, %{
             alpn: [~c"anoma v1"],
             verify: :none
           }) do
      {:ok,
       %QuicConnection{
         router: router,
         transport: transport,
         mode: :client,
         conn: conn
       }}
    end
  end

  # listen for connection on existing server
  def init({:listener, router, transport, listener}) do
    with {:ok, ^listener} <- :quicer.async_accept(listener, %{}) do
      {:ok,
       %QuicConnection{
         router: router,
         transport: transport,
         mode: :listener,
         listener: listener
       }}
    end
  end

  def handle_cast({:send, msg}, _, s) do
    :quicer.send(s.stream, msg)
    {:noreply, s}
  end

  def handle_cast(:shutdown, _, s) do
    :quicer.async_shutdown_connection(s.conn, 0, 0)
    {:noreply, s}
  end

  def handle_info({:quic, :transport_shutdown, _, _}, s) do
    die(s, "transport shutdown")
  end

  def handle_info({:quic, :closed, _, _}, s) do
    die(s, "connection closed")
  end

  # client: connection succeeded
  def handle_info(
        {:quic, :connected, _, _},
        s = %QuicConnection{mode: :client}
      ) do
    case :quicer.start_stream(s.conn, %{active: true}) do
      {:ok, stream} ->
        Transport.new_connection(s.transport, :quic)
        {:noreply, %{s | stream: stream}}

      {:error, _, err} ->
        die(s, Atom.to_string(err))

      {:error, err} ->
        die(s, Atom.to_string(err))
    end
  end

  # server: new connection from client; start handshake and start a new listener
  def handle_info({:quic, :new_conn, conn, _}, s) do
    start_listener(s)

    case :quicer.async_handshake(conn) do
      :ok ->
        {:noreply, %{s | conn: conn}}

      {:error, err} ->
        die(s, to_string(err))
    end
  end

  # server: completed handshake; connection now established.  wait for stream from client
  def handle_info(
        {:quic, :connected, _, _},
        s = %QuicConnection{mode: :listener}
      ) do
    case :quicer.async_accept_stream(s.conn, %{}) do
      {:ok, _} ->
        {:noreply, s}

      {:error, err} ->
        die(s, to_string(err))
    end
  end

  # server: got stream.  connection is now (putatively) completely operational
  def handle_info(
        {:quic, :new_stream, stream, _},
        s = %QuicConnection{mode: :listener}
      ) do
    Transport.new_connection(s.transport, :quic)
    {:noreply, %{s | stream: stream}}
  end

  # client or server (at this point, the distinction is meaningless): received some data
  def handle_info({:quic, data, stream, _}, s)
      when stream == s.stream and is_binary(data) do
    Transport.receive_chunk(s.transport, data)
    {:noreply, s}
  end

  # unknown message?
  def handle_info({:quic, a, b, c}, s) do
    # Logger.info("quic msg #{inspect(a)} #{inspect(b)} #{inspect(c)}")
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
      {:listener, s.router, s.transport, s.listener}
    )
  end
end
