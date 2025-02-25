defmodule Anoma.Node.Transport.TCP.Listener do
  @moduledoc """
  I am a TCP listener. I listen on a given interface and port for incoming connections.

  When I receive a connection, I create a new TCPServer process to further handle the connection.

  ### Public API

  I provide the following public functionality:

  - `port/1`
  """

  alias Anoma.Node.Registry
  alias Anoma.Node.Transport.TCP

  require Logger

  use GenServer
  use TypedStruct

  @typedoc """
  Shorthand type for socket.
  """
  @type hostname :: :inet.socket_address() | :inet.hostname()

  @typedoc """
  Shorthand type for port number.
  """
  @type port_number :: :inet.port_number()

  ############################################################
  #                    State                                 #
  ############################################################

  typedstruct do
    @typedoc """
    I am the state of a TCP listener.

    My fields contain information to listen for TCP connection with a remote node.

    ### Fields
    - `:node_id`        - The key of this router. This value is used to announce myself to other
    - `:host`           - The host address of the remote tcp server.
    - `:port`           - The port of the remote tcp server.
    - `:socket`         - The socket of an accepted connection.
    """
    field(:node_id, String.t())
    field(:host, hostname)
    field(:port, port_number)
    field(:socket, :inet.socket())
  end

  ############################################################
  #                    Genserver Helpers                     #
  ############################################################

  @doc """
  I am the child spec for a TCP listener.

  I ensure that TCP listeners are not restarted if they terminate.
  """
  @spec child_spec([any()]) :: Supervisor.child_spec()
  def child_spec(args) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [args]},
      restart: :temporary
    }
  end

  @spec start_link([any()]) :: GenServer.on_start()
  def start_link(args) do
    GenServer.start_link(__MODULE__, args)
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @doc """
  I return the port on which I am listening.
  This is useful for the examples where I start up using a random port.
  """
  @spec port(GenServer.server()) :: port_number()
  def port(tcp_server) do
    GenServer.call(tcp_server, :port)
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  @doc """
  I am the init function for a TCP listener process.

  ### Options

    - `:host`    - The host to whom I will listen.
    - `:port`    - The port on which I will listen.
    - `:node_id` - The key of the local node.
  """
  @impl true
  def init(args) do
    Process.set_label(__MODULE__)

    Logger.debug("starting tcp listener with #{inspect(args)}")

    # trap exits to shut down cleanly.
    Process.flag(:trap_exit, true)

    # validate the arguments given to me and use them to create my initial state.
    args = Keyword.validate!(args, [:host, :port, :node_id])
    state = struct(__MODULE__, Enum.into(args, %{}))

    {:ok, state, {:continue, :create_socket}}
  end

  @impl true
  # @doc """
  # Create a new socket to listen for incoming connections.
  # If the port was 0, this will use a random free port.
  # """
  def handle_continue(:create_socket, state) do
    case create_socket(state.host, state.port) do
      {:ok, socket, port_number} ->
        Logger.debug("tcp server on port #{port_number}")
        new_state = %{state | socket: socket, port: port_number}
        {:noreply, new_state, {:continue, :accept_connections}}

      {:error, reason} ->
        Logger.error("failed to create listening socket: #{inspect(reason)}")
        state = handle_fatal_error(reason, state)
        {:stop, :normal, state}
    end
  end

  # @doc """
  # Start accepting connections.
  # """
  def handle_continue(:accept_connections, state) do
    async_accept(state.socket, state.node_id)
    {:noreply, state}
  end

  # ----------------------------------------------------------------------------
  # Calls

  @doc """
  I return the port on which I am listening
  """
  @impl true
  def handle_call(:port, _from, state) do
    {:reply, state.port, state}
  end

  # ----------------------------------------------------------------------------
  # Casts

  @impl true
  def handle_cast(:accept, state) do
    async_accept(state.socket, state.node_id)
    {:noreply, state}
  end

  # @doc """
  # I receive this message when the async accept process failed to accept a socket.
  # Can be safely ignored.
  # """
  def handle_cast(:accept_failure, state) do
    async_accept(state.socket, state.node_id)
    {:noreply, state}
  end

  # ----------------------------------------------------------------------------
  # Infos

  @impl true
  # @doc """
  # I receive this message when the async accept process stops.
  # Can be safely ignored.
  # """
  def handle_info({:EXIT, _, :normal}, state) do
    {:noreply, state}
  end

  def handle_info(m, state) do
    Logger.warning("unhandled message: #{inspect(m)}")
    {:noreply, state}
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  # @doc """
  # I handle a fatal error in the TCP connection.
  # A fatal error is an error the tcp connection does not wish to recover from.
  # """
  @spec handle_fatal_error(any(), t()) :: t()
  defp handle_fatal_error(reason, state) do
    Logger.error("fatal error in #{__MODULE__}: #{inspect(reason)}")
    state
  end

  ############################################################
  #                           Helpers                        #
  ############################################################

  # @doc """
  # I create a new socket on an interface and port.
  # if the socket is created, I return the socket.
  # If something went wrong when creating the socket, I return an error.
  # """
  @spec create_socket(hostname(), port_number()) ::
          {:ok, :inet.socket(), port_number()} | {:error, any()}
  defp create_socket(host, port) do
    # start the sockets in passive mode (i.e., no messages)
    listen_options = [
      :binary,
      active: false,
      exit_on_close: true,
      reuseaddr: true,
      ifaddr: host
    ]

    case :gen_tcp.listen(port, listen_options) do
      {:ok, listen_socket} ->
        {:ok, port} = :inet.port(listen_socket)
        {:ok, listen_socket, port}

      {:error, reason} ->
        {:error, reason}
    end
  end

  # @doc """
  # I fire up a new task to accept a new connection on the socket.
  # I immediately return.
  # """
  @spec async_accept(:inet.socket(), String.t()) :: :ok
  defp async_accept(socket, node_id) do
    this = self()

    # wait in an external process to free up the tcp server for introspection
    # e.g., get the current port
    spawn_link(fn ->
      case wait_for_accept(socket, node_id) do
        {:ok, _} ->
          GenServer.cast(this, :accept)

        {:error, _} ->
          GenServer.cast(this, :accept_failure)
      end
    end)

    :ok
  end

  # @doc """
  # I wait for a new incoming connection to accept.
  # If the socket is accepted, I put to passive to avoid data coming in.
  # I then create a process for this connection and give it ownership of the socket.
  # I then put the socket back to active mode so it will send its data to the new process
  # instead of me.
  # If something went wrong, I return an error.
  # """
  @spec wait_for_accept(:inet.socket(), String.t()) ::
          {:ok, :accepted} | {:error, :failed_to_accept}
  defp wait_for_accept(socket, node_id) do
    case :gen_tcp.accept(socket) do
      # a new connection was accepted
      {:ok, socket} ->
        # create a new listener process for the next connection
        {:ok, pid} = create_new_connection(socket, node_id)

        # change the owner of the socket and put it back to active mode.
        :gen_tcp.controlling_process(socket, pid)
        :inet.setopts(socket, [{:active, true}])
        {:ok, :accepted}

      # the socket was closed by us.
      {:error, :closed} ->
        {:error, :failed_to_accept}
    end
  end

  # @doc """
  # I create a new connection process to manage a new tcp connection.
  # if the tcp connection process is created, I return its pid.
  # If anything goes wrong, I return an error.
  # """
  @spec create_new_connection(:inet.socket(), String.t()) ::
          {:ok, pid()} | {:error, any()}
  defp create_new_connection(socket, node_id) do
    supervisor = Registry.whereis(node_id, :tcp_supervisor)

    args = [node_id: node_id, socket: socket]

    DynamicSupervisor.start_child(supervisor, {TCP.Connection, args})
  end
end
