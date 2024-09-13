defmodule Anoma.Node.Transport.TCPConnection do
  @moduledoc """
  I am TCPConnection Engine.

  I manage an individual TCP connection and stream.

  I can run in two modes: either I can act as a client, initiating a connection
  to a server, or as a listener, waiting to accept and then accepting a
  connection on a server.
  In the latter case, since accepting the connection marks me as its owner and
  handoff is complicated, I start up a new listener once I accept a connection.


  ### How I work

  A good general overview of how I work at a high level with my
  environment can be seen in `Anoma.Node.Transport.TCPServer`.

  The diagrams I'll include in my documentation are more focused on my D2D inner workings

  ```mermaid
  graph LR;

  %% Role Setup
  TCPConnection:::TCPConnection
  Child_Process(Child Connection):::Listener
  Listener1(A TCP Listener Node):::Listener
  Transport(Our Transport):::Transport


  %% Note Relationship between TCPConnection and the outside
  :accept_connection -- start_listener --- Child_Process
  Transport -- handshake ---Listener1


  subgraph TCPConnection
    direction TB
    :accept_connection:::Listener
    :init_connection:::Client

    init -- is a Listener -->:accept_connection
    init -- is a Client -->:init_connection

    :accept_connection -- ":gen_tcp.accept" --- :accept_connection


    :accept_connection & :init_connection -- failure --> sd(Shut down)
    :accept_connection & :init_connection -- successful --> Standby

  end


  %% Note Relationship between TCPConnection and the outside
  :init_connection -- ":gen_tcp.connect" --- Listener1
  :init_connection -- begin handshake ---Transport



  %% Styling
  classDef Listener      fill:#add8e6
  classDef TCPConnection fill:#fff9ca
  classDef Client        fill:#e6add8
  classDef Transport     fill:#d8e6ad


  %% Linking

  click Transport "https://anoma.github.io/anoma/Anoma.Node.Transport.html"
  click init "https://anoma.github.io/anoma/Anoma.Node.Transport.TCPConnection.html#init/1"
  click Child_Process "https://anoma.github.io/anoma/Anoma.Node.Transport.TCPConnection.html"
  click :init_connection "https://anoma.github.io/anoma/Anoma.Node.Transport.TCPConnection.html"
  click :accept_connection "https://anoma.github.io/anoma/Anoma.Node.Transport.TCPConnection.html"
  click Listener1 "https://anoma.github.io/anoma/Anoma.Node.Transport.TCPConnection.html"
  ```

  This diagram uses the following Color Codes:
  1. Blue Nodes represent TCP Connections running in the listening mode.
  2. Purple Node represents TCP Connection running in the client mode.
  3. Green Nodes is the Transport Server.

  We can see that my behavior differs drastically if I'm in the client
  mode or a listening mode.

  If I'm listening then I'll block calling `:gen_tcp.accept/1`, if
  that works, then we create another
  `Anoma.Node.Transport.TCPConnection`. If not we shutdown

  Likewise for the client behavior, we try using
  `:gen_tcp.connect/3`. If this fails then we shutdown.

  If we do successfully connect, then we begin the handshake process,
  which is best explained in a diagram in `Anoma.Node.Transport`.

  """

  alias Anoma.Node.Router
  alias Anoma.Node.Transport
  alias Anoma.Node.Logger
  alias __MODULE__

  use Transport.Connection
  use TypedStruct

  @num_connect_retries 8

  @typedoc """
  I am the type of :gen_tcp.connect() result.
  """
  @type tcp_connect_result ::
          {:ok, :gen_tcp.socket()} | {:error, :timeout | :inet.posix()}

  typedstruct do
    @typedoc """
    I am the type of the TCPConnection Engine.

    ### Fields
    - `:router` - The address of the Router Engine that the Transport Engine
      instance serves to.
    - `:transport` - The address of the Transport server managing the
      connection.
    - `:connection_pool` - The supervisor which manages the connection pool that
      the TCPConnection Engine instance belongs to.
    - `:mode` - The mode of the connection: client or listener.
    - `:listener` - The listening socket that accepts incoming connection
      requests. Must be provided in the listener mode. Default: nil
    - `:conn` - Socket of the established connection for the listener mode.
      Initially, nil. Filled in as soon as a connection is established.
    - `:logger` - The Logger Engine address. Enforced: false.
    """
    field(:router, Router.addr())
    field(:transport, Router.addr())
    field(:connection_pool, Supervisor.supervisor())
    field(:mode, :client | :listener)
    field(:listener, reference() | nil)
    field(:conn, reference() | nil)
    field(:logger, Router.addr(), enforce: false)
  end

  # TODO: annoyingly, we can't initiate tcp connections asynchronously, so it's
  # not clear how to cleanly abort the connection attempt
  # still, we can initiate the connection in a continue, so we don't block
  # whoever started us

  @doc """
  I am the initialization function for TCPConnection Engine.

  ### Pattern-Matching Variations

  - `init({:client, router, transport, address, connection_pool, logger})` -
    create a TCP connection as a client.

  - `init({:listener, router, transport, listener, connection_pool, logger})` -
    create a TCP connection as a listener.
  """
  @spec init(
          {:client, Router.addr(), Router.addr(), Transport.transport_addr(),
           Supervisor.supervisor(), Router.addr()}
        ) :: {:ok, t(), {:continue, {:init_connection, any()}}}
  def init({:client, router, transport, address, connection_pool, logger}) do
    {:ok,
     %TCPConnection{
       router: router,
       transport: transport,
       connection_pool: connection_pool,
       mode: :client,
       logger: logger
     }, {:continue, {:init_connection, address}}}
  end

  @spec init(
          {:listener, Router.addr(), Router.addr(), reference(),
           Supervisor.supervisor(), Router.addr()}
        ) :: {:ok, t(), {:continue, :accept_connection}}
  def init({:listener, router, transport, listener, connection_pool, logger}) do
    {:ok,
     %TCPConnection{
       router: router,
       transport: transport,
       connection_pool: connection_pool,
       mode: :listener,
       listener: listener,
       logger: logger
     }, {:continue, :accept_connection}}
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_continue({:init_connection, address}, s) do
    logger = s.logger
    start_connecting_task(address, self(), logger)

    {:noreply, s}
  end

  def handle_continue(:accept_connection, s) do
    start_accepting_task(s.listener, self())

    {:noreply, s}
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

  def handle_info({_, {:accept_done, res}}, s) do
    case res do
      {:ok, conn} ->
        start_listener(s)
        # need to figure out if unix or tcp
        Transport.new_connection(s.transport, :unix)
        {:noreply, %{s | conn: conn}}

      {:error, :closed} ->
        die(s, inspect(:closed))

      {:error, reason} ->
        logger = s.logger
        log_info({:error_accept, reason, logger})
        die(s, inspect(reason))
    end
  end

  def handle_info({_, {:connect_done, address, res}}, s) do
    case res do
      {:ok, conn} ->
        Transport.new_connection(
          s.transport,
          Transport.transport_type(address)
        )

        {:noreply, %{s | conn: conn}}

      {:error, reason} ->
        log_info({:error_connect, reason, s.logger})
        die(s, inspect(reason))
    end
  end

  def handle_info({:tcp_closed, _}, s) do
    die(s, "connection shutdown")
  end

  def handle_info({:tcp, _, data}, s) do
    Transport.receive_chunk(s.transport, data)
    {:noreply, s}
  end

  # handle DOWN message from the accepting / connecting task
  def handle_info({:DOWN, _, _, _, _}, s) do
    {:noreply, s}
  end

  # handle EXIT message from the accepting / connecting task
  def handle_info({:EXIT, _, _}, s) do
    {:noreply, s}
  end

  def terminate(_, s) do
    if s.conn do
      :gen_tcp.shutdown(s.conn, :read_write)

      receive do
        {:tcp_closed, _} ->
          Transport.disconnected(s.transport, "connection shutdown")
      end
    else
      Transport.disconnected(s.transport, "connection shutdown")
    end
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  defp start_connecting_task(address, control_proc, logger) do
    Task.async(fn ->
      res = try_connect(address, logger)

      case res do
        {:ok, conn} ->
          :gen_tcp.controlling_process(conn, control_proc)

        _ ->
          nil
      end

      {:connect_done, address, res}
    end)
  end

  defp start_accepting_task(listen_sock, control_proc) do
    Task.async(fn ->
      res = :gen_tcp.accept(listen_sock)

      case res do
        {:ok, conn} ->
          :gen_tcp.controlling_process(conn, control_proc)

        _ ->
          nil
      end

      {:accept_done, res}
    end)
  end

  defp die(s, reason) do
    Transport.disconnected(s.transport, reason)
    {:stop, :normal, s}
  end

  defp start_listener(s) do
    Router.start_engine(
      s.router,
      __MODULE__,
      {:listener, s.router, s.transport, s.listener, s.connection_pool,
       s.logger},
      supervisor: s.connection_pool
    )
  end

  @spec try_connect(Transport.transport_addr(), Router.addr()) ::
          tcp_connect_result()
  defp try_connect(address, logger) do
    0..(@num_connect_retries - 1)
    |> Enum.reduce_while(nil, fn x, _acc ->
      do_backoff(x)

      case tcp_connect(address, logger) do
        {:ok, socket} -> {:halt, {:ok, socket}}
        {:error, :timeout} -> {:cont, {:error, :timeout}}
        # TODO refine the case
        {:error, err_posix} -> {:halt, {:error, err_posix}}
      end
    end)
  end

  ############################################################
  #                          Helpers                         #
  ############################################################

  @spec do_backoff(non_neg_integer()) :: :ok
  defp do_backoff(i) do
    if i > 0 do
      Integer.pow(2, i - 1)
      |> :timer.seconds()
      |> :timer.sleep()
    end
  end

  @spec tcp_connect(Transport.transport_addr(), Router.addr()) ::
          tcp_connect_result()
  defp tcp_connect({:unix, path}, logger) do
    log_info({:connect_unix, path, logger})
    :gen_tcp.connect({:local, path}, 0, mode: :binary)
  end

  defp tcp_connect({:tcp, host, port}, logger) do
    host = to_charlist(host)
    log_info({:connect_tcp, host, port, logger})
    :gen_tcp.connect(host, port, mode: :binary)
  end

  ############################################################
  #                     Logging Info                         #
  ############################################################

  defp log_info({:error_connect, reason, logger}) do
    Logger.add(
      logger,
      :error,
      "Failed to connect to a socket: #{inspect(reason)}."
    )
  end

  defp log_info({:error_accept, reason, logger}) do
    Logger.add(
      logger,
      :error,
      "Failed to accept connection to a socket: #{inspect(reason)}."
    )
  end

  defp log_info({:connect_unix, path, logger}) do
    Logger.add(
      logger,
      :info,
      "Connecting to a Unix socket on path #{path}"
    )
  end

  defp log_info({:connect_tcp, host, port, logger}) do
    Logger.add(
      logger,
      :info,
      "Connecting to a TCP socket on #{inspect(host)}:#{inspect(port)}"
    )
  end
end
