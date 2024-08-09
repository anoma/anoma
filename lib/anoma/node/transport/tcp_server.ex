defmodule Anoma.Node.Transport.TCPServer do
  alias Anoma.Node.{Router, Transport, Logger}
  alias __MODULE__

  use Transport.Server
  use TypedStruct

  require Logger

  typedstruct do
    field(:router, Router.addr())
    field(:transport, Router.addr())
    field(:connection_pool, Supervisor.supervisor())
    field(:listener, reference())
    field(:logger, Router.addr(), enforce: false)
  end

  def init({router, transport, addr, connection_pool, logger}) do
    res =
      case addr do
        {:unix, path} ->
          log_info({:listen_unix, path, logger})
          :gen_tcp.listen(0, ifaddr: {:local, path}, mode: :binary)

        # todo bind host
        {:tcp, _host, port} ->
          log_info({:listen_port, port, logger})
          :gen_tcp.listen(port, mode: :binary)
      end

    case res do
      {:ok, l} ->
        {:ok,
         %TCPServer{
           router: router,
           transport: transport,
           listener: l,
           connection_pool: connection_pool
         }, {:continue, :start_listener}}

      {:error, reason} ->
        log_info({:error, reason, logger})
        {:error, reason}
    end
  end

  def handle_continue(:start_listener, s) do
    Router.start_engine(
      s.router,
      Transport.TCPConnection,
      {:listener, s.router, s.transport, s.listener, s.connection_pool},
      supervisor: s.connection_pool
    )

    {:noreply, s}
  end

  ############################################################
  #                     Logging Info                         #
  ############################################################

  defp log_info({:error, reason, logger}) do
    Logger.add(
      logger,
      :error,
      "Failed to create a listen socket: #{inspect(reason)}."
    )
  end

  defp log_info({:listen_unix, path, logger}) do
    Logger.add(
      logger,
      :info,
      "Creating a listen Unix socket on path #{path}"
    )
  end

  defp log_info({:listen_port, port, logger}) do
    Logger.add(
      logger,
      :info,
      "Creating a listen socket on port #{inspect(port)}."
    )
  end
end
