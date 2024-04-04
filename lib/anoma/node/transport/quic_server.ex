defmodule Anoma.Node.Transport.QuicServer do
  @moduledoc """
  I am a QUIC server; I don't do much currently, as the connection management
  is all in QuicConnection.
  """

  alias Anoma.Node.Router
  alias Anoma.Node.Transport
  alias __MODULE__

  use Transport.Server
  use TypedStruct

  require Logger

  typedstruct do
    field(:router, Router.addr())
    field(:transport, Router.addr())
    field(:listener, reference())
  end

  def init({router, transport}) do
    init({router, transport, 24768, "0.0.0.0"})
  end

  def init({router, transport, port, host}) do
    init({router, transport, port, host, ~c"cert.pem", ~c"key.pem"})
  end

  # todo: to improve scalability, we could have multiple listeners running
  # concurrently, but there is no point at all in doing that before we stop
  # serialising everything though a single transport/router
  # todo: allow the server to be shut down
  def init({router, transport, port, _host, cert, key}) do
    with {:ok, listener} <-
           :quicer.listen(
             port,
             %{
               certfile: cert,
               keyfile: key,
               alpn: [~c"anoma v1"],
               peer_bidi_stream_count: 1
             }
           ) do
      Router.start_engine(
        router,
        Transport.QuicConnection,
        {:listener, router, transport, listener}
      )

      {:ok,
       %QuicServer{router: router, transport: transport, listener: listener}}
    end
  end

  def handle_info({:quic, a, b, c}, s) do
    Logger.info("quic server #{inspect(a)} #{inspect(b)} #{inspect(c)}")
    {:noreply, s}
  end
end
