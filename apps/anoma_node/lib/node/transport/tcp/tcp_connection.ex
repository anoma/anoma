defmodule Anoma.Node.Transport.TCP.Connection do
  @moduledoc """
  I am the TCP connection process. I manage a tcp connection with a remote node.
  """
  alias Anoma.Node.Transport
  alias Anoma.Node.Transport.Messages
  alias Anoma.Protobuf.Announce.Announcement
  alias Anoma.Protobuf.Envelope
  alias EventBroker.Event

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
    I am the state of a TCP connection.

    My fields contain information to facilitate the TCP connection with a remote node.

    ### Fields
    - `:socket`         - The socket of the connection.
    - `:node_id`        - The key of this router. This value is used to announce myself to other
                          nodes.
    """
    field(:socket, port())
    field(:node_id, String.t())
    field(:callers, %{GenServer.from() => binary()}, default: %{})
  end

  ############################################################
  #                    Genserver Helpers                     #
  ############################################################

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
  #                    Genserver Behavior                    #
  ############################################################

  @impl true
  # @doc """
  # I don't do anything useful.
  # """
  def init(args) do
    Process.set_label(__MODULE__)

    Logger.debug("#{inspect(self())} starting tcp connection")
    args = Keyword.validate!(args, [:node_id, :socket])
    state = struct(__MODULE__, Enum.into(args, %{}))

    {:ok, state, {:continue, :announce}}
  end

  @impl true
  # @doc """
  # I announce the current node to the socket I connected to.
  # """
  def handle_continue(:announce, state) do
    Logger.debug("#{inspect(self())} announcing node")

    message = Messages.announcement(state.node_id)
    GenServer.cast(self(), {:tcp_out, message})
    {:noreply, state}
  end

  @impl true
  # @doc """
  # I send the given bytes over the socket.
  # and reply when the response has arrived.
  # """
  def handle_call({:tcp_out, message}, from, state) do
    handle_tcp_out(message, state.socket)
    # remember the caller and message ref to send back the reply
    callers = Map.put(state.callers, message.message_id, from)
    state = %{state | callers: callers}

    # do not reply here, reply when the response arrives
    {:noreply, state}
  end

  @impl true
  # @doc """
  # I send the given bytes over the socket and do not reply.
  # """
  def handle_cast({:tcp_out, message}, state) do
    handle_tcp_out(message, state.socket)
    {:noreply, state}
  end

  # @doc """
  # I reply to a message that was sent previoulsy.
  # """
  def handle_cast({:reply_to, ref, message}, state) do
    case Map.pop(state.callers, ref) do
      {nil, _} ->
        Logger.error("no caller found for ref #{inspect(ref)}")
        {:noreply, state}

      {message_id, callers} ->
        GenServer.reply(message_id, message)
        {:noreply, %{state | callers: callers}}
    end
  end

  @impl true
  # @doc """
  # I handle an incoming TCP message from the socket.
  # """
  def handle_info({:tcp, _, bytes}, state) do
    handle_tcp_in(bytes, state.node_id)
    {:noreply, state}
  end

  # @doc """
  # I handle a closed socket.
  # When the socket is closed I start the reconnect process.
  # """
  def handle_info({:tcp_closed, _}, state) do
    Logger.warning("tcp client socket closed")
    {:stop, :normal, state}
  end

  ############################################################
  #                           Helpers                        #
  ############################################################

  # @doc """
  # I handle a bunch of incoming bytes over the TCP socket.

  # I decode them into a protobuf message and handle them accordingly.
  # """
  @spec handle_tcp_in(any(), String.t()) :: :ok
  defp handle_tcp_in(bytes, local_node_id) do
    Logger.debug("tcp in :: #{inspect(self())} :: #{inspect(bytes)}")

    case decode(bytes) do
      {:ok, message} ->
        Logger.debug("tcp in :: #{inspect(self())} :: #{inspect(message)}")
        handle_message_in(message, local_node_id)

      {:error, _} ->
        Logger.error("invalid message")
    end
  end

  # @doc """
  # I handle a protobuf message that has to be sent over the wire.

  # I encode the message into byts and push them onto the socket.
  # """
  @spec handle_tcp_out(Envelope.t(), port()) :: :ok
  defp handle_tcp_out(message, socket) do
    Logger.debug("tcp out :: #{inspect(self())} :: #{inspect(message)}")
    bytes = encode(message)
    :gen_tcp.send(socket, bytes)
  end

  ############################################################
  #                  Encoding/Decoding                       #
  ############################################################

  # @doc """
  # I decode a binary into a protobuf message, if possible.
  # """
  @spec decode(binary()) :: {:ok, Envelope.t()} | {:error, :invalid_message}
  defp decode(bytes) do
    try do
      {:ok, Envelope.decode(bytes)}
    rescue
      _e -> {:error, :invalid_message}
    end
  end

  # @doc """
  # I encode a protobuf Envelope into bytes.
  # """
  @spec encode(Envelope.t()) :: binary()
  defp encode(message = %Envelope{}) do
    Protobuf.encode(message)
  end

  ############################################################
  #                           Messages                       #
  ############################################################

  # @doc """
  # I handle any incoming message after it has been decoded.
  #
  # If the message is an announcement, I setup the engine proxy.
  #
  # Every other message is forwarded to the engine proxy.
  # """
  defp handle_message_in(message = %Envelope{}, local_node_id) do
    {_, inner_message} = message.inner_message
    handle_message_in(inner_message, message.message_id, local_node_id)
  end

  defp handle_message_in(announcement = %Announcement{}, _ref, local_node_id) do
    Logger.debug("received announcement :: #{inspect(announcement)}")
    remote_node_id = announcement.node_info

    announcement.engines
    |> Enum.each(
      &register_proxy(local_node_id, remote_node_id, String.to_atom(&1))
    )
  end

  defp handle_message_in(message, ref, local_node_id) do
    Logger.debug("received message for node :: #{inspect(message)}")

    case Messages.proto_to_call(message, ref, local_node_id) do
      {:reply, env = %Envelope{}} ->
        GenServer.cast(self(), {:tcp_out, env})
        :ok

      {:is_reply, reply, message_id} ->
        GenServer.cast(self(), {:reply_to, message_id, reply})
    end
  end

  ############################################################
  #                           Helpers                        #
  ############################################################

  # @doc """
  # Given a node id and a type, I create an engine proxy for this remote node its engine.
  # """
  defp register_proxy(local_node_id, remote_node_id, type) do
    # it is assumed that the proxy always starts, or already exists.
    # todo: do away with this assumption
    case Transport.start_engine_proxy(local_node_id, remote_node_id, type) do
      {:error, {:already_started, pid}} ->
        Logger.debug("proxy already started :: #{inspect(pid)}")

      {:ok, pid} ->
        Logger.debug("proxy started :: #{inspect(pid)}")
    end

    # register this tcp connection as transport for this engine
    name = Anoma.Node.Registry.address(remote_node_id, type, :transport)
    Elixir.Registry.register(Anoma.Node.Registry, name, nil)

    # send an event to announce the discovery of a new engine
    %Event{
      source_module: __MODULE__,
      body: {:new_node_discovered, remote_node_id}
    }
    |> EventBroker.event()
  end
end
