defmodule Anoma.Node.Transport.TCP.Client do
  @moduledoc """
  I am a TCP client. I connect to a remote TCP server and send messages to it.

  I decode the incoming data and encode outgoing data.
  """
  use GenServer
  use TypedStruct

  require Logger

  alias Anoma.Crypto.Id

  import Anoma.Node.Transport.TCP.Shared

  @num_connect_retries 8

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
    I am the state of a TCP client.

    My fields contain information to facilitate the TCP connection with a remote node.

    ### Fields
    - `:host`           - The host address of the remote tcp server.
    - `:port`           - The port of the remote tcp server.
    - `:socket`         - The socket of the connection.
    - `:node_id`        - The key of this router. This value is used to announce myself to other
                          nodes.
    """
    field(:host, hostname())
    field(:port, port_number())
    field(:socket, port())
    field(:node_id, Id.t())
  end

  ############################################################
  #                    Genserver Helpers                     #
  ############################################################

  @doc """
  I am the child spec for a TCP client.

  I ensure that TCP clients are not restarted if they terminate.
  """
  def child_spec(args) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [args]},
      restart: :temporary
    }
  end

  def start_link(args) do
    args = Keyword.validate!(args, [:node_id, :host, :port])

    GenServer.start_link(__MODULE__, args)
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  @doc """
  I am the init function for a TCP client connection.

  I expect a host, port, and local router key to initialize the connection.

  ### Options

    - `:host`    - The host to whom I will connect.
    - `:port`    - The port on which I will connect.
    - `:node_id` - The key of the local node.
  """
  @impl true
  def init(args) do
    Logger.debug("starting tcp client with #{inspect(args)}")

    # trap exits to shut down cleanly.
    Process.flag(:trap_exit, true)

    # validate the arguments given to me and use them to create my initial state.
    args = Keyword.validate!(args, [:host, :port, :node_id])
    state = struct(__MODULE__, Enum.into(args, %{}))

    {:ok, state, {:continue, :connect}}
  end

  @impl true
  # @doc """
  # I get called after the init function and I set up the state.
  # """
  def handle_continue(:connect, state) do
    send(self(), {:try_connect, @num_connect_retries})
    {:noreply, state}
  end

  @impl true
  # @doc """
  # I am called when the genserver terminates.
  # """
  def terminate(reason, _state) do
    Logger.warning("tcp client terminated #{inspect(reason)}")
    :ok
  end

  # ----------------------------------------------------------------------------
  # Casts

  # @doc """
  # I send a message to the remote node.
  # I'm usually called from the proxy engine.
  # """
  @impl true
  def handle_cast({:send, message}, state) do
    case handle_send(message, state.socket) do
      {:ok, :sent} ->
        {:noreply, state}

      {:error, :failed_to_send} ->
        state = handle_fatal_error(:failed_to_send_message, state)
        {:stop, :normal, state}
    end
  end

  # ----------------------------------------------------------------------------
  # Infos

  @impl true
  # @doc """
  # I get called when there have been `@num_connect_retries` attempts to connect.
  # At this point the connection will not be retried and the client stops.
  # """
  def handle_info({:try_connect, 0}, state) do
    state = handle_fatal_error(:attempts_failed, state)
    {:stop, :normal, state}
  end

  # @doc """
  # I get called when there are still attempts left to connect.
  # I will try to connect to the remote host and port.
  # If the connection is successful, I will do the handshake process.
  # If the attempt fails, I will try again after a backoff interval.
  # Any other error reason is fatal and I fail normally.
  # """
  def handle_info({:try_connect, attempts}, state) do
    case try_connect(state.host, state.port) do
      # connection successful
      {:ok, socket} ->
        handle_connection_established(self(), state.node_id)
        {:noreply, %{state | socket: socket}}

      # failed to connect
      {:error, _} ->
        Logger.debug("failed connection attempt, retrying")

        message = {:try_connect, attempts - 1}
        timeout = backoff_interval(attempts)
        Process.send_after(self(), message, timeout)

        {:noreply, state}
    end
  end

  # @doc """
  # I handle an incoming TCP message from the socket.
  # """
  def handle_info({:tcp, _, bytes}, state) do
    handle_received_bytes(bytes, state.node_id, connection_type: __MODULE__)
    {:noreply, state}
  end

  # @doc """
  # I handle a closed socket.
  # When the socket is closed I start the reconnect process.
  # """
  def handle_info({:tcp_closed, _}, state) do
    Logger.debug("tcp client socket closed")
    send(self(), {:try_connect, @num_connect_retries})
    {:noreply, state}
  end

  # @doc """
  # Catch all for unknown messages
  # """
  def handle_info(_, state) do
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
    Logger.error("fatal error in tcp client (#{inspect(reason)})")

    state
  end

  ############################################################
  #                           Helpers                        #
  ############################################################

  # @doc """
  # I calculate the backoff interval for the given attempt.
  # I return the interval in milliseconds.
  # """
  @spec backoff_interval(integer) :: integer
  defp backoff_interval(attempt) do
    Integer.pow(2, attempt - 1)
    |> :timer.seconds()
  end

  # @doc """
  # I try and connect over TCP to the given host and port.
  # If the connection succeeds I return the socket, otherwise I return an error.
  # """
  @spec try_connect(hostname(), port_number()) ::
          {:ok, port()} | {:error, :timeout} | {:error, any()}
  defp try_connect(host, port) do
    case :gen_tcp.connect(host, port, mode: :binary) do
      {:ok, socket} ->
        :inet.setopts(socket, [{:active, true}])
        {:ok, socket}

      {:error, e} ->
        {:error, e}
    end
  end
end
