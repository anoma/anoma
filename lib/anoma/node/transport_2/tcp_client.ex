defmodule Anoma.Node.Transport2.TCPClient do
  @moduledoc """
  I am a TCP client. I connect to a remote TCP server and send messages to it.

  I decode the incoming data and encode outgoing data.
  """
  use GenServer
  use TypedStruct

  require Logger

  alias __MODULE__
  alias Anoma.Crypto.Id
  alias Anoma.Node.Transport2.Discovery

  import Anoma.Node.Transport2.MessageEncoding

  @num_connect_retries 8

  ############################################################
  #                    State                                 #
  ############################################################

  typedstruct do
    @typedoc """
    I am the state of a TCP client.

    My fields contain information to facilitate the TCP connection with a remote node.

    ### Fields
    - `:host`       - The host address of the remote tcp server.
    - `:port`       - The port of the remote tcp server.
    - `:socket`     - The socket of the connection.
    - `:router_key` - The key of this router. This value is used to announce myself to other
                      nodes.
    """
    field(:host, :inet.socket_address() | :inet.hostname())
    field(:port, :inet.port_number())
    field(:socket, port())
    field(:router_key, Id.t())
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
    GenServer.start_link(__MODULE__, args)
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  @doc """
  I am the init function for a TCP client connection.

  I expect a host, port, and local router key to initialize the connection.
  """
  @impl true
  @spec init(
          host: :inet.socket_address() | :inet.hostname(),
          port: :inet.port_number(),
          router_key: Id.t()
        ) :: {:ok, t(), {:continue, :connect}}
  def init(host: host, port: port, router_key: router_key) do
    state = %TCPClient{host: host, port: port, router_key: router_key}
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

  # @doc """
  # I get called when there have been `@num_connect_retries` attempts to connect.
  # At this point the connection will not be retried and the client stops.
  # """
  @impl true
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
        handle_connection_established(state)
        {:noreply, %{state | socket: socket}}

      # failed to connect
      {:error, :timeout} ->
        Logger.debug("failed connection attempt, retrying")
        message = {:try_connect, attempts - 1}
        timeout = backoff_interval(attempts)
        Process.send_after(self(), message, timeout)

        {:noreply, state}

      # other errors are fatal
      {:error, reason} ->
        state = handle_fatal_error(reason, state)
        {:stop, :normal, state}
    end
  end

  # ----------------------------------------------------------
  # tcp messages

  # @doc """
  # I handle an incoming TCP message from the socket.
  # """
  def handle_info({:tcp, _, bytes}, state) do
    Logger.debug(">> #{inspect(self())} :: #{inspect(bytes)}")
    {:ok, state} = handle_bytes(bytes, state)
    {:noreply, state}
  end

  # @doc """
  # I handle an outgoing TCP message.
  # Before sending the message I encode it.
  # If the message fails to send, I terminate the connection normally.
  # """
  def handle_info({:send, message}, state) do
    Logger.debug("<< #{inspect(self())} :: #{inspect(message)}")

    case :gen_tcp.send(state.socket, encode_message(message)) do
      :ok ->
        {:noreply, state}

      {:error, err} ->
        Logger.debug("failed to send bytes to socket, terminating")
        state = handle_fatal_error(err, state)
        {:stop, :normal, state}
    end
  end

  # @doc """
  # I handle a closed socket.
  # When the socket is closed I stop the connection normally.
  # """
  def handle_info({:tcp_closed, _}, state) do
    Logger.debug("#{inspect(self())} :: socket closed")
    state = handle_fatal_error(:socket_closed, state)
    {:stop, :normal, state}
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  # @doc """
  # I process an incoming message from the socket.
  # I decode it and then handle the decoded message.
  # If the message cannot be decoded, I just ignore it.
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
  # If the message is an inform message I remember the node.
  # """
  @spec handle_message(term()) :: :ok
  defp handle_message(message) do
    Logger.debug("decoded message: #{inspect(message)}")

    case message do
      %{type: :inform} ->
        Discovery.remember_node(message.router_id)
        Discovery.register_as_tcp_for_node(message.router_id)

      _ ->
        :ok
    end

    :ok
  end

  # @doc """
  # I handle a successful connection to the remote node.
  # I inform the node my of myself.
  # """
  @spec handle_connection_established(t()) :: :ok
  defp handle_connection_established(state) do
    Discovery.inform_node(self(), state.router_key)
    :ok
  end

  ############################################################
  #                           Connecting                     #
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
  @spec try_connect(
          :inet.socket_address() | :inet.hostname(),
          :inet.port_number()
        ) ::
          {:ok, port()} | {:error, :timeout} | {:error, any()}
  defp try_connect(host, port) do
    case :gen_tcp.connect(host, port, mode: :binary) do
      {:ok, socket} ->
        :inet.setopts(socket, [{:active, true}])
        {:ok, socket}

      {:error, :timeout} ->
        {:error, :timeout}

      {:error, reason} ->
        {:error, reason}
    end
  end

  # @doc """
  # I handle a fatal error in the TCP connection.
  # A fatal error is an error the tcp connection does not wish to recover from.
  # """
  @spec handle_fatal_error(any(), t()) :: t()
  defp handle_fatal_error(reason, state) do
    Logger.debug(
      "failed connection to #{inspect(state.host)}:#{state.port} (#{inspect(reason)})"
    )

    state
  end
end
