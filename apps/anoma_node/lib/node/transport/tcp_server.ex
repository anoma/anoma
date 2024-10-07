defmodule Anoma.Node.Transport.TCPServer do
  @moduledoc """
  I am a TCP server. I listen on a given interface and port for incoming connections.

  I decode the incoming data and encode outgoing data.
  """

  use GenServer
  use TypedStruct

  require Logger

  alias __MODULE__
  alias Anoma.Crypto.Id
  alias Anoma.Node.Transport.Discovery
  alias Anoma.Node.Transport.TCPServer
  alias Anoma.Node.Transport.TCPSupervisor

  import Anoma.Node.Transport.MessageEncoding

  ############################################################
  #                    State                                 #
  ############################################################

  typedstruct do
    @typedoc """
    I am the state of a TCP server.

    My fields contain information to facilitate the TCP connection with a remote node.

    ### Fields
    - `:host`          - The host address of the remote tcp server.
    - `:port`          - The port of the remote tcp server.
    - `:listen_socket` - The socket of the listening server.
    - `:socket`        - The socket of an accepted connection.
    - `:router_key`    - The key of this router. This value is used to announce myself to other
                         nodes.
    """
    field(:host, :inet.socket_address() | :inet.hostname())
    field(:port, :inet.port_number())
    field(:listen_socket, :inet.socket())
    field(:connection_socket, :inet.socket())
    field(:router_key, Id.t())
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
  """
  def start_link(args) do
    GenServer.start_link(TCPServer, args)
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  @impl true
  @doc """
  I am the init function for a TCP server.

  I expect a host, port, and local router key to initialize the connection.

  If I am called for a new TCP server, I start up a socket and start accepting connections.
  If I am called for an accepted connection, I wait for incoming messages.

  ### Pattern-Matching Variations

  - `init([host: host, port: port, router_key: router_key])`
    I initialize a new TCP server with the given host, port and key.

  - `init(%TCPServer{})`:
    I initialize a new TCP connection with the given state.
  """
  def init(host: host, port: port, router_key: router_key) do
    Process.flag(:trap_exit, true)
    Logger.debug("starting tcp server #{inspect(host)}:#{port}")

    state = %TCPServer{host: host, port: port, router_key: router_key}

    # create a socket and start accepting connections
    case create_socket(state) do
      {:ok, listen_socket} ->
        {:ok, port} = :inet.port(listen_socket)
        Logger.debug("listening on port #{port}")

        new_state = %{state | listen_socket: listen_socket, port: port}

        send(self(), :accept)

        {:ok, new_state}

      {:error, :failed_to_create_socket, reason} ->
        Logger.error("failed to create listening socket: #{inspect(reason)}")

        _state = handle_fatal_error(reason, state)
        {:stop, :normal}
    end
  end

  def init(%TCPServer{connection_socket: _socket} = state) do
    Process.flag(:trap_exit, true)
    Logger.debug("starting connection process")

    send(self(), :connected)

    {:ok, state}
  end

  @impl true
  # @doc """
  # I am called when the genserver terminates.
  # """
  def terminate(reason, _state) do
    Logger.warning("terminated #{inspect(reason)}")
    :ok
  end

  # @doc """
  # I send a message to the remote noe.
  # I'm called from the proxy engine.
  # """
  @impl true
  def handle_cast({:send, message}, state) do
    Logger.debug("<< #{inspect(message)}")

    case handle_send(message, state) do
      {:ok, :sent} ->
        {:noreply, state}

      {:error, :failed_to_send} ->
        state = handle_fatal_error(:failed_to_send_message, state)
        {:stop, :normal, state}
    end
  end

  @impl true
  # @doc """
  # I get called to wait for a new incoming connection.
  # When a new connection is made, I start a new server and wait for the next connection.
  # If anything goes wrong while accepting, I terminate the server normally.
  # """
  def handle_info(:accept, state) do
    case wait_for_accept(state) do
      {:ok, new_state} ->
        send(self(), :accept)
        {:noreply, new_state}

      {:error, new_state} ->
        new_state = handle_fatal_error(:accept_failure, new_state)
        {:stop, :normal, new_state}
    end
  end

  # @doc """
  # I get called when a new connection has been established.
  # I do the initial handshaking and discovery process.
  # """
  def handle_info(:connected, state) do
    Logger.debug("client connected")
    handle_connection_established(state)
    {:noreply, state}
  end

  # ----------------------------------------------------------
  # tcp messages

  # @doc """
  # I handle an incoming TCP packet.
  # """
  def handle_info({:tcp, _, bytes}, state) do
    Logger.debug(">> #{inspect(bytes)}")
    {:ok, state} = handle_bytes(bytes, state)
    {:noreply, state}
  end

  # @doc """
  # I handle the outgoing tcp messages.
  # I first encode the message, and then send the bytes over the write.
  # """
  def handle_info({:send, message}, state) do
    Logger.debug("<< #{inspect(self())} :: #{inspect(message)}")

    case handle_send(message, state) do
      {:ok, :sent} ->
        {:noreply, state}

      {:error, :failed_to_send} ->
        state = handle_fatal_error(:failed_to_send_message, state)
        {:stop, :normal, state}
    end
  end

  # @doc """
  # I handle the closed socket.
  # I typically get called when the remote side closes the socket.
  # If this happens I stop the connection normally.
  # """
  def handle_info({:tcp_closed, _}, state) do
    Logger.debug("socket closed")
    state = handle_fatal_error(:tcp_closed, state)
    {:stop, :normal, state}
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  # @doc """
  # I send a message over the socket to the other node.
  # """
  @spec handle_send(term(), t()) :: {:ok, :sent} | {:error, :failed_to_send}
  defp handle_send(message, state) do
    case :gen_tcp.send(state.connection_socket, encode_message(message)) do
      :ok ->
        {:ok, :sent}

      {:error, _} ->
        {:error, :failed_to_send}
    end
  end

  # @doc """
  # I handle an incoming binary by first decoding it and then handling the message.
  # """
  @spec handle_bytes(binary, t()) :: {:ok, t()}
  defp handle_bytes(bytes, state) do
    case decode_bytes(bytes) do
      {:ok, message} ->
        handle_message(message)
        {:ok, state}

      {:error, :failed_to_decode} ->
        {:ok, state}
    end
  end

  # @doc """
  # I handle a decoded message.
  # If the message is a discovery message, I remember the node.
  # """
  @spec handle_message(term()) :: :ok
  defp handle_message(message) do
    Logger.debug("decoded message: #{inspect(message)}")

    case message do
      %{type: :inform} ->
        Discovery.remember_node(message.router_id)
        Discovery.register_as_tcp_for_node(message.router_id)
        :ok

      _ ->
        :ok
    end
  end

  # @doc """
  # I handle a new succesful connection.
  # When this happens, I inform the remote node about my existence.
  # Afterwards I register myself as the TCP connection for the remote node.
  # """
  @spec handle_connection_established(t()) :: :ok
  defp handle_connection_established(state) do
    Discovery.inform_node(self(), state.router_key)
    :ok
  end

  ############################################################
  #                           Helpers                        #
  ############################################################

  # @doc """
  # I create a new socket on an interface and port.
  # if the socket is created, I return the socket.
  # If something went wrong when creating the socket, I return an error.
  # """
  @spec create_socket(t()) ::
          {:ok, :inet.socket()} | {:error, :failed_to_create_socket, any()}
  defp create_socket(state) do
    case :gen_tcp.listen(state.port, ifaddr: state.host, mode: :binary) do
      {:ok, listen_socket} ->
        {:ok, listen_socket}

      {:error, reason} ->
        {:error, :failed_to_create_socket, reason}
    end
  end

  # @doc """
  # I create a new connection process to manage a new tcp connection.
  # if the tcp connection process is created, I return its pid.
  # If anything goes wrong, I return an error.
  # """
  @spec create_new_connection(t()) :: {:ok, pid()} | {:error, any()}
  defp create_new_connection(state) do
    case DynamicSupervisor.start_child(TCPSupervisor, {TCPServer, state}) do
      {:ok, pid} ->
        {:ok, pid}

      {:ok, pid, _} ->
        {:ok, pid}

      err ->
        {:error, :failed_to_start_connection, err}
    end
  end

  # @doc """
  # I wait for a new incoming connection to accept.
  # If the socket is accepted, I put to passive to avoid data coming in.
  # I then create a process for this connection and give it ownership of the socket.
  # I then put the socket back to active mode so it will send its data to the new process
  # instead of me.
  # If something went wrong, I return an error.
  # """
  @spec wait_for_accept(t()) :: {:ok, t()} | {:error, t()}
  defp wait_for_accept(state) do
    case :gen_tcp.accept(state.listen_socket) do
      # a new connection was accepted
      {:ok, socket} ->
        # set the socket to passive to avoid data coming in
        # while its being handed over to another process
        :inet.setopts(socket, [{:active, false}])

        # create a new listener process for the next connection
        {:ok, pid} =
          create_new_connection(%{
            state
            | connection_socket: socket,
              listen_socket: nil
          })

        # change the owner of the socket and put it back to active mode.
        :gen_tcp.controlling_process(socket, pid)
        :inet.setopts(socket, [{:active, true}])
        {:ok, state}

      # the socket was closed by us.
      {:error, :closed} ->
        {:error, state}
    end
  end

  # @doc """
  # I handle a fatal error in the TCP connection.
  # A fatal error is an error the tcp connection does not wish to recover from.
  # """
  @spec handle_fatal_error(any(), t()) :: t()
  defp handle_fatal_error(reason, state) do
    Logger.error(
      "failed connection to #{inspect(state.host)}:#{state.port} (#{inspect(reason)})"
    )

    state
  end
end
