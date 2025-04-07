defmodule Anoma.Node.Transport.Proxy.Node do
  @moduledoc """
  I am the node proxy. I am responsible for communication with a remote node.

  https://specs.anoma.net/v0.1.3/arch/node/net/node_proxy.html

  A Node Proxy engine is responsible for communication with one specific remote
  node.

  It performs transport selection, connection establishment and maintenance.

  It forwards messages between local engine instances and Transport Connection
  engine instances.

  Connections may be ephemeral or permanent. Ephemeral connections are
  established when the first message is sent to the node, or when the remote
  node initiates a connection, and not re-established automatically when the
  connection is lost. Permanent connections are established when the Node Proxy
  is started, and automatically re-established when the connection is lost.

  The engine instance name corresponds to the remote NodeID.
  """
  alias Anoma.Node.Event
  alias Anoma.Node.Intents.IntentPool.Events.IntentAddSuccess
  alias Anoma.Node.Registry
  alias Anoma.Node.Transaction.Mempool.Events.TxFilter
  alias Anoma.Node.Transport.Proxy.Events
  alias Anoma.Node.Transport.Proxy.TransportProtocol

  use EventBroker.DefFilter
  use GenServer
  use TypedStruct

  require Logger

  @args [:node_id, :transport, :remote_node_id]

  ############################################################
  #                       Event Filter                       #
  ############################################################

  # This filter will filter out all events that are not explicitly marked as
  # external. An external event can be sent to other nodes over the network, but
  # is not guaranteed to be delivered exactly once. Internal events are, so they
  # should never be sent over the wire.

  deffilter ExcludeNode, node_id: String.t() do
    # external events from the remote node
    %EventBroker.Event{
      body: %Event{
        body: %Events.External{},
        node_id: ^node_id
      }
    } ->
      false

    # external events from any other node
    %EventBroker.Event{
      body: %Event{
        body: %Events.External{}
      }
    } ->
      true

    # ignore all other events
    _e ->
      false
  end

  ############################################################
  #                       Types                              #
  ############################################################

  @typep startup_options() :: [
           {:node_id, String.t()},
           {:remote_node_id, String.t()}
         ]

  ############################################################
  #                       State                              #
  ############################################################

  typedstruct enforce: true do
    @typedoc """
    I am the type of the PubSub Enigine.

    ### Fields

    - `:node_id` - The ID of the Node to which I belong.

    """
    field(:node_id, String.t())
    field(:remote_node_id, String.t())
  end

  ############################################################
  #                      GenServer Callbackse                #
  ############################################################

  @doc """
  I am the start_link function for the Ordering Engine.

  I register the engine with supplied node ID provided by the arguments.
  """

  @spec start_link(startup_options) :: GenServer.on_start()
  def start_link(args \\ []) do
    args = Keyword.validate!(args, @args)

    name = Registry.via(args[:remote_node_id], __MODULE__)
    GenServer.start_link(__MODULE__, args, name: name)
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @spec subscribe(String.t(), String.t()) ::
          :ok
          | {:error, :invalid_topic}
          | {:error, :could_not_subscribe, String.t()}
  def subscribe(node_id, topic) do
    GenServer.call(Registry.via(node_id, __MODULE__), {:subscribe, topic})
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  @impl true
  @doc """
  """
  def init(args) do
    Logger.debug("#{inspect(self())} proxy node for #{inspect(args)}")
    Process.set_label(__MODULE__)

    args = Keyword.validate!(args, @args)
    state = struct(__MODULE__, Enum.into(args, %{}))

    # subscribe to all the events happening on this node,
    # except the ones pertaining to the remote node.
    # this will cause infinite loops.
    EventBroker.subscribe_me([
      %ExcludeNode{node_id: state.remote_node_id}
    ])

    {:ok, state}
  end

  @impl true
  def handle_call({:subscribe, topic}, _from, state) do
    with {:ok, filters} <- topic_to_filter(state.node_id, topic),
         :ok <- EventBroker.subscribe_me(filters) do
      {:reply, :ok, state}
    else
      {:error, :invalid_topic} ->
        {:reply, {:error, :invalid_topic}, state}

      err when is_binary(err) ->
        {:reply, {:error, :could_not_subscribe, err}, state}
    end
  end

  def handle_call(_request, _from, state) do
    {:reply, :ok, state}
  end

  @impl true
  def handle_info(event = %EventBroker.Event{}, state) do
    # determine the topic the event belongs to
    topic = filter_to_topic(event)
    remote_node_id = state.remote_node_id

    # create the event struct that contains all information required to get
    # this to the remote node
    message = %{topic: topic, event: event}

    case list_transport_protocols(remote_node_id) do
      [] ->
        Logger.error("no protocols for #{remote_node_id}")
        {:reply, :ok, state}

      [transport_protocol | _] ->
        address = Registry.via(transport_protocol)
        result = TransportProtocol.event(address, message)
        {:reply, result, state}
    end

    {:noreply, state}
  end

  ############################################################
  #                    Genserver Helpers                     #
  ############################################################

  # @doc """
  # I locate a TransportProtocol process for the remote node.
  # """
  @spec list_transport_protocols(String.t()) :: [Registry.Address.t()]
  defp list_transport_protocols(node_id) do
    Registry.match(node_id, TransportProtocol)
  end

  @spec topic_to_filter(String.t(), String.t()) ::
          {:ok, [any()]} | {:error, :invalid_topic}
  defp topic_to_filter(node_id, "*") do
    # this filter subscribes to all events pertaining to this particular node
    {:ok, [Event.node_filter(node_id)]}
  end

  @spec topic_to_filter(String.t(), String.t()) :: {:ok, [term()]}
  defp topic_to_filter(node_id, "tx_events") do
    # this filter subscribes to all transaction events pertaining to this
    # particular node
    {:ok, [Event.node_filter(node_id), %TxFilter{}]}
  end

  defp topic_to_filter(_, _) do
    # this filter subscribes to all transaction events pertaining to this
    # particular node
    {:error, :invalid_topic}
  end

  # @doc """
  # Given an event, I determine which topic it belongs to.
  # """
  @spec filter_to_topic(term()) :: String.t()
  defp filter_to_topic(event) do
    case event do
      %{body: %{body: %IntentAddSuccess{}}} ->
        "tx_event"

      _ ->
        "*"
    end
  end
end
