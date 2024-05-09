defmodule Anoma.Node.Transport.TCPServer do
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

  def init({router, transport, addr}) do
    res =
      case addr do
        {:unix, path} ->
          :gen_tcp.listen(0, ifaddr: {:local, path}, mode: :binary)

        # todo bind host
        {:tcp, _host, port} ->
          :gen_tcp.listen(port, mode: :binary)
      end

    with {:ok, l} <- res do
      {:ok, %TCPServer{router: router, transport: transport, listener: l},
       {:continue, :start_listener}}
    end
  end

  def handle_continue(:start_listener, s) do
    Router.start_engine(
      s.router,
      Transport.TCPConnection,
      {:listener, s.router, s.transport, s.listener}
    )

    {:noreply, s}
  end
end
