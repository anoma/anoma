defmodule Anoma.Node.Transport.Quic do
  use TypedStruct

  require Logger

  alias Anoma.Node.Router
  alias Anoma.Crypto.Id
  alias __MODULE__

  typedstruct do
    field(:router, Router.addr())
    #field(:topic, Router.addr())
    field(:transport, Router.addr())
    field(:listener, reference())
    # outgoing connections.  values are {conn, stream, child_pid}
    field(:connections, %{Id.Extern.t() => {reference(), reference()}}, default: %{})
    # stream => {id, length-or-nil, in-progress-data}
    field(:connection_streams, %{reference => {Id.Extern.t, non_neg_integer() | nil, String.t()}}, default: %{})
  end

  def init({router, transport}) do init({router, transport, 24768, "0.0.0.0"}) end

  def init({router, transport, port, host}) do init({router, transport, port, host, 'cert.pem', 'key.pem'}) end

  def init({router, transport, port, _host, cert, key}) do
    {:ok, listener} = :quicer.listen(port, [certfile: cert, keyfile: key, alpn: ['anoma v1'], peer_bidi_stream_count: 1])
    {:ok, listener} = :quicer.async_accept(listener, %{})
    {:ok, %Quic{router: router, transport: transport, listener: listener}}
  end

  #verb == :connected or
  def handle_info({:quic, verb, conn, _}, s) when verb == :new_conn do
    {:ok, conn} = :quicer.handshake(conn)
    {:ok, stream} = :quicer.accept_stream(conn, %{})
    intro = receive do {:quic, intro, ^stream, _} -> intro end
    Logger.info("intro #{inspect(intro)}")
    # todo there should be an actual handshake with nonces and whatnot
    {:intro, node_id = %Id.Extern{}} = :erlang.binary_to_term(intro)
    Router.cast(s.transport, {:connected, node_id, :quic})
    # todo handle duplicate connections

    # ???
    {:ok, _listener} = :quicer.async_accept(s.listener, %{})

    {:noreply, %{s |
      connections: Map.put(s.connections, node_id, {conn, stream}),
      connection_streams: Map.put(s.connection_streams, stream, {node_id, nil, ""})}}
    #-> Router.start_engine(s.router, Anoma.Node.Transport.Quic.Connection, {s.router, s.transport, conn})
    #case :quicer.handshake(conn) do
    #  err -> Logger.warning("failed to accept connection: #{inspect(err)}")
    #end
    #:erlang.monitor(:process, spawn(fn -> handle_conn(s.router, conn, Router.self_addr(s.router)) end))
    #{:noreply, s}
  end
  def handle_info({:quic, msg, stream, extra}, s) when is_map_key(s.connection_streams, stream) and is_binary(msg) do
    {id, length, data} = Map.fetch!(s.connection_streams, stream)
    Logger.info("received message from #{inspect(id)}: #{inspect(extra)}")
    {length, data} = unframe(s.router, id, length, data <> msg)
    {:noreply, %{s | connection_streams: %{s.connection_streams | stream => {id, length, data}}}}
  end
  def handle_info({:quic, a, b, c}, s) do
    Logger.info("quic msg #{inspect(a)} #{inspect(b)} #{inspect(c)}")
    {:noreply, s}
  end
  def handle_cast({:send, id, msg}, _, s) when is_map_key(s.connections, id) do
    send_to_connection(elem(Map.fetch!(s.connections, id), 1), msg)
    {:noreply, s}
  end

  def unframe(router, id, length, data) do
    Logger.info("unframe length data #{inspect(length)} #{inspect(byte_size(data))}")
    if length == nil do
      if byte_size(data) >= 4 do
        unframe(router, id,
          :binary.decode_unsigned(binary_part(data, 0, 4), :little),
          binary_part(data, 4, byte_size(data)-4))
      else
        {length, data}
      end
    else
      if byte_size(data) >= length do
        Logger.info("complete message of size #{inspect(length)}")
        Router.cast(router, {:p2p_raw, id, binary_part(data, 0, length)})
        unframe(router, id, nil, binary_part(data, length, byte_size(data)-length))
      else
        {length, data}
      end
    end
  end

  def send_to_connection(stream, data) do
    # pad length to 4 bytes
    length = :binary.encode_unsigned(byte_size(data), :little)
    length = length <> :binary.copy(<<0>>, 4 - byte_size(length))
    Logger.info("sending length #{inspect(byte_size(data))} #{inspect(length)}")
    :quicer.send(stream, length <> :binary.copy(<<0>>, 4 - byte_size(length)))
    :quicer.send(stream, data)
  end

  #def handle_cast({:connected, id, conn, stream, pid}, _, s) do
  #  # if we have an existing connection, kill it
  #  case Map.get(s.connections, id) do
  #    nil -> nil
  #    {conn, _, pid} ->
  #      Process.exit(pid, :kill)
  #      :ok = :quicer.shutdown_connection(conn, :infinity)
  #      Router.cast(s.topic, {:node_disconnected, id})
  #  end
  #  Router.cast(s.topic, {:node_connected, id})
  #  {:noreply, %{s | connections: %{s.connections | id => {conn, stream, pid}}}}
  #end
end
