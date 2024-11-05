defmodule EventBroker.Broker do
  @moduledoc """
  I am a Broker module.

  I specify the behavior of the server acting as a central broker of the
  PubSub service. My functionality is minimal. I wait for messages and
  relay them to my subscribers.
  """

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

  @spec start_link(list()) :: GenServer.on_start()
  def start_link(args \\ []) do
    GenServer.start_link(__MODULE__, %EventBroker.Broker{},
      name: args[:broker_name] || __MODULE__
    )
  end

  @impl true
  def init(_opts) do
    {:ok, %EventBroker.Broker{}}
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  @impl true
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

  @impl true
  def handle_cast({:event, event}, state) do
    handle_info(event, state)
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end

  @impl true
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
