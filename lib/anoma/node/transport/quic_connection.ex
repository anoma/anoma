defmodule Anoma.Node.Transport.Quic.Connection do
  use TypedStruct
  require Logger
  alias Anoma.Node.Router
  alias Anoma.Crypto.Id
  alias __MODULE__

  typedstruct do
    field(:router, Router.addr())
    field(:transport, Router.addr())
    field(:conn, reference())
    field(:stream, reference())
    field(:node_id, Id.Extern.t())

    # queue
    field(:length, nil | non_neg_integer(), default: nil)
    field(:data, String.t(), default: "")
  end

  # initiate outgoing connection
  def init({router, transport, other_id, {:quic, host, port}}) do
    {:ok, conn} = :quicer.connect(host, port, [alpn: ['anoma v1'], verify: :none], 5000)
    {:ok, stream} = :quicer.start_stream(conn, [])
    {:ok, _} = :quicer.send(stream, :erlang.term_to_binary({:intro, router.id}))
    Logger.info("opened quic connection to #{inspect(host)} #{inspect(port)}")
    {:ok, %Connection{router: router, transport: transport, conn: conn, stream: stream, node_id: other_id}}
  end

  # todo we should plumb timeouts throughout here (config parameters?).  and probably also limit active connections or somethign
  #def init({router, transport, conn}) do
  #  Logger.info("new connection #{inspect(conn)}")
  #  #{:ok, conn} = :quicer.handshake(conn)
  #  {:ok, stream} = :quicer.accept_stream(conn, %{})
  #  Logger.info("stream #{inspect(stream)}")
  #  intro = receive do {:quic, intro, ^stream, _} -> intro end
  #  Logger.info("intro #{inspect(intro)}")
  #  # todo there should be an actual handshake with nonces and whatnot
  #  {:intro, node_id = %Id.Extern{}} = :erlang.binary_to_term(intro)
  #  Router.cast(transport, {:connected, node_id, :quic})
  #  {:ok, %Connection{router: router, transport: transport, conn: conn, stream: stream, node_id: node_id}}
  #end

  def handle_info({:quic, data, stream, _}, s) when stream == s.stream and is_binary(data) do
    Logger.info("received message from #{inspect(s.node_id)}")
    {length, data} = Anoma.Node.Transport.Quic.unframe(s.router, s.node_id, s.length, s.data <> data)
    {:noreply, %{s | length: length, data: data}}
  end
  def handle_info({:quic, a, b, c}, s) do
    Logger.info("quic msg #{inspect(a)} #{inspect(b)} #{inspect(c)}")
    {:noreply, s}
  end
  def handle_cast({:send, _id, msg}, _, s) do
    # todo nicer error handling/etc.
    Logger.info("sending to #{inspect(s.node_id)}")
    Anoma.Node.Transport.Quic.send_to_connection(s.stream, msg)
    {:noreply, s}
  end
end
