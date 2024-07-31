defmodule EventBroker do
  @moduledoc """
  I am an Event Broker module.

  I specify the behavior of the server acting as a central broker of the
  PubSub servive. My functionality is minimal. I wait for messages and
  relay them to my subscribers.

  We assume that there is only one local entity named EventBroker for
  proper functionality.

  ### Public API

  I provide the following public functionality:

  - `event/1`
  """

  alias __MODULE__

  use GenServer
  use TypedStruct

  typedstruct enforce: true do
    @typedoc """
    I am the type of the Event Broker.

    ### Fields

    - `:subscribers` - The set of pids showcasing subscribers.
                       Default: Map.Set.new()
    """

    field(:subscribers, MapSet.t(pid()), default: MapSet.new())
  end

  def start_link() do
    GenServer.start_link(__MODULE__, %EventBroker{}, name: __MODULE__)
  end

  def init(_opts) do
    {:ok, %EventBroker{}}
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @doc """
  I am the Event Broker event function.

  I process any incoming events by sending them to all of my subscribers
  using the `send/2` functionality.
  """

  @spec event(EventBroker.Event.t()) :: :ok
  def event(event = %EventBroker.Event{}) do
    GenServer.cast(__MODULE__, {:event, event})
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_call({:subscribe, pid}, _from, state) do
    {:reply, :ok, %{state | subscribers: MapSet.put(state.subscribers, pid)}}
  end

  def handle_call({:unsubscribe, pid}, _from, state) do
    {:reply, :ok,
     %{state | subscribers: MapSet.delete(state.subscribers, pid)}}
  end

  def handle_call(_msg, _from, state) do
    {:reply, :ok, state}
  end

  def handle_cast({:event, event}, state) do
    handle_info(event, state)
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end

  def handle_info(event = %EventBroker.Event{}, state) do
    for pid <- state.subscribers do
      send(pid, event)
    end

    {:noreply, state}
  end

  def handle_info(_info, state) do
    {:noreply, state}
  end
end
