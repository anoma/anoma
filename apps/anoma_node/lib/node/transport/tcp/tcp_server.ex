defmodule Anoma.Node.Transport.TCP.Server do
  @moduledoc """
  I am a TCP server. I manage a tcp connection with a client.

  I decode the incoming data and encode outgoing data.
  """

  use GenServer
  use TypedStruct

  require Logger

  alias Anoma.Crypto.Id

  import Anoma.Node.Transport.TCP.Shared

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
    I am the state of a TCP server.

    My fields contain information to facilitate the TCP connection with a remote node.

    ### Fields
    - `:socket`         - The socket of an accepted connection.
    - `:node_id`        - The key of this router. This value is used to announce myself to other
                          nodes.
    """
    field(:socket, :inet.socket())
    field(:node_id, Id.t())
  end

  ############################################################
  #                    Genserver Helpers                     #
  ############################################################

  @doc """
  I am the child spec for a TCP server.

  I ensure that TCP server are not restarted if they terminate.
  """

  def child_spec(args) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [args]},
      restart: :temporary
    }
  end

  @doc """
  I create a new TCP server based on a host, port and key.

  ### Options

    - `:socket`  - The socket over which I will communicate.
    - `:node_id` - The key of the local node.
  """
  def start_link(args) do
    args = Keyword.validate!(args, [:node_id, :socket])

    # note: tcp servers are not registered because they are not
    # uniquely labeled. Many tcp server can exist with the same
    # host and port, given that the port can be 0.
    # Once a connection with a node has been established, they
    # can be identified uniquely.

    GenServer.start_link(__MODULE__, args)
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  @impl true
  @doc """
  I am the init function for a TCP server.

  I expect a host, port, and local router key to initialize the connection.

  If I am called for a new TCP server, I start up a socket and start accepting connections.
  If I am called for an accepted connection, I wait for incoming messages.

  ### Options

    - `:socket`  - The socket over which I will communicate.
    - `:node_id` - The key of the local node.

  ### Pattern-Matching Variations

  - `init([host: host, port: port, node_id: node_id])`
    I initialize a new TCP server with the given host, port and key.

  - `init(%TCPServer{})`:
    I initialize a new TCP connection with the given state.
  """
  def init(args) do
    Logger.debug("starting connection process #{inspect(self())}")

    # trap exits to shut down cleanly.
    Process.flag(:trap_exit, true)

    # validate the arguments given to me and use them to create my initial state.
    args = Keyword.validate!(args, [:node_id, :socket])
    state = struct(__MODULE__, Enum.into(args, %{}))

    {:ok, state, {:continue, :connected}}
  end

  @impl true
  # @doc """
  # I get called when the init function is done. I handle the logic
  # for a fresh connection.
  # """
  def handle_continue(:connected, state) do
    handle_connection_established(self(), state.node_id)
    {:noreply, state}
  end

  @impl true
  # @doc """
  # I am called when the genserver terminates.
  # """
  def terminate(reason, _state) do
    Logger.warning("tcp server terminated #{inspect(reason)}")
    :ok
  end

  # ----------------------------------------------------------------------------
  # Casts

  # @doc """
  # I receive this message to send it over the wire to the remote node.
  # I'm sent by the proxy engine.
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

  # ----------------------------------------------------------
  # Infos

  @impl true
  # @doc """
  # I handle an incoming TCP packet.
  # """
  def handle_info({:tcp, _, bytes}, state) do
    handle_received_bytes(bytes, state.node_id, connection_type: __MODULE__)
    {:noreply, state}
  end

  # @doc """
  # I handle the closed socket.
  # I typically get called when the remote side closes the socket.
  # If this happens I stop the connection normally.
  # """
  def handle_info({:tcp_closed, _}, state) do
    Logger.debug("tcp server socket closed")
    state = handle_fatal_error(:tcp_closed, state)
    {:stop, :normal, state}
  end

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
    Logger.error("fatal error in tcp server (#{inspect(reason)})")

    state
  end
end
