defmodule Anoma.Node.Transport.TCPServer do
  @moduledoc """
  I am TCPServer Engine.

  I open a listening TCP connection in server mode on the specified address.
  """
  alias Anoma.Node.{Router, Transport, Logger}
  alias __MODULE__

  use Transport.Server
  use TypedStruct

  require Logger

  typedstruct do
    @typedoc """
    I am the type of the TCPServer Engine.

    ### Fields
    - `:router` - The address of the Router Engine that the Transport Engine
      instance serves to.
    - `:transport` - The address of the Transport server managing the server.
    - `:connection_pool` - The supervisor which manages the connection pool that
      the TCPServer Engine instance belongs to.
    - `:listener` - The socket listening on the specified host, port.
      request.  Must be provided in the listener mode. Default: nil
    - `:logger` - The Logger Engine address. Enforced: false.
    """
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

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

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
