defmodule EventBroker.FilterAgent do
  @moduledoc """
  I am a Filter Agent module.

  I implement the base server behavior of the spawned filter agent. In
  general, I monitor subscribers of an individual filtering agent and
  send whichever events I receive to them.
  """

  alias __MODULE__

  use GenServer, restart: :transient
  use TypedStruct

  typedstruct enforce: true do
    @typedoc """
    I am the type of the Filter Agent.

    I contain the minimal info for a filter to work, namely the filter
    specification to apply to incoming messages and subscribers to send
    filtered messages to.

    ### Fields

    - `:spec` - The filter specification. This is a structure of a module
                with a public filter API for the agent to call.
    - `:subscribers` - The list of subscribers to send filtered messages to.
                       Default: MapSet.new()
    """

    field(:spec, struct())
    field(:subscribers, MapSet.t(pid()), default: MapSet.new())
  end

  @spec start_link(struct()) :: GenServer.on_start()
  def start_link(filter_params) do
    GenServer.start_link(__MODULE__, filter_params)
  end

  @impl true
  def init(filter_params) do
    {:ok,
     %FilterAgent{
       spec: filter_params
     }}
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  @impl true
  def handle_call({:subscribe, pid}, _from, state) do
    {:reply, :ok, %{state | subscribers: MapSet.put(state.subscribers, pid)}}
  end

  def handle_call({:unsubscribe, pid}, _from, state) do
    new_subs = MapSet.delete(state.subscribers, pid)

    new_state = %{state | subscribers: new_subs}

    if Enum.empty?(new_subs) do
      {:stop, :normal, :reap, new_state}
    else
      {:reply, :ok, new_state}
    end
  end

  def handle_call(_msg, _from, state) do
    {:reply, :ok, state}
  end

  @impl true
  def handle_cast(_msg, state) do
    {:noreply, state}
  end

  @impl true
  def handle_info(event = %EventBroker.Event{}, state) do
    if state.spec.__struct__.filter(event, state.spec) do
      for pid <- state.subscribers do
        send(pid, event)
      end
    end

    {:noreply, state}
  end

  def handle_info(_info, state) do
    {:noreply, state}
  end
end
