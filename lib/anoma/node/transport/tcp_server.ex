defmodule Anoma.Node.Transport.TCPServer do
  @moduledoc """
  I am TCPServer Engine.

  I open a listening TCP connection in server mode on the specified address.

  ### How I work

  A good way to understand how I work is by looking at the diagram of my behavior below:

  ```mermaid
  graph TB;
  Client1(Client 1):::Client
  Client2(Client 2):::Client
  subgraph Dynamic Supervisor
  TCPServer:::Server-- :start_listener---Conn1
  Conn1(Listener 1):::Connection-- start_listener---Conn2
  Conn2(Listener 2):::Connection-- start_listener---Conn3
  Conn3(Listener 3):::Connection-- :gen_tcp.accept---Conn3
  end
  Client1-. ":gen_tcp.connect()" .->Conn1
  Client2-. ":gen_tcp.connect()" .->Conn2

  classDef Server      fill:#d8e6ad
  classDef Connection  fill:#add8e6
  classDef Client      fill:#e6add8

  click TCPServer "https://anoma.github.io/anoma/Anoma.Node.Transport.TCPServer.html" "Anoma.Node.Transport.TCPServer"

  click Conn1 "https://anoma.github.io/anoma/Anoma.Node.Transport.TCPConnection.html" "Anoma.Node.Transport.TCPConnection"
  click Conn2 "https://anoma.github.io/anoma/Anoma.Node.Transport.TCPConnection.html" "Anoma.Node.Transport.TCPConnection"
  click Conn3 "https://anoma.github.io/anoma/Anoma.Node.Transport.TCPConnection.html" "Anoma.Node.Transport.TCPConnection"

  click Client1 "https://anoma.github.io/anoma/Anoma.Node.Transport.TCPConnection.html" "Anoma.Node.Transport.TCPConnection"
  click Client2 "https://anoma.github.io/anoma/Anoma.Node.Transport.TCPConnection.html" "Anoma.Node.Transport.TCPConnection"

  click Dynamic Supervisor "https://anoma.github.io/anoma/Anoma.Node.Transport.Supervisor.html"
  ```

  This diagram uses the following Color Codes:
  1. Blue Nodes represent TCP Connections running in the listening mode.
  2. Purple Nodes represent TCP Connections running in the client mode.
  3. Green Node is the TCP Server.

  Here we can see that I manage the TCP Server by spawning
  `Anoma.Node.Transport.TCPConnection` instances that other TCP Clients can
  talk to.

  See `Anoma.Node.Transport.TCPConnection` for further information on
  the specifics for how my connections work.

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

  @spec init(
          {Router.addr(), Router.addr(), Transport.listen_addr(),
           Supervisor.supervisor(), Router.addr()}
        ) :: {:ok, t()} | {:error, any()}
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
  #                      Public RPC API                      #
  ############################################################

  @doc """
  I expose the listener of the server
  """
  def listener(server) do
    Router.call(server, :listener)
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

  def handle_call(:listener, _from, s) do
    {:reply, s.listener, s}
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
