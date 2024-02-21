defmodule Anoma.Node.Control.Measure.Communicator do
  alias __MODULE__

  use TypedStruct
  use GenServer

  alias Anoma.Node.Control
  alias Anoma.Node.Control.Measure.Engine
  alias Anoma.Node.Utility

  typedstruct do
    field(:engine, GenServer.server(), enforce: true)

    field(:subscribers, MapSet.t({GenServer.server(), atom()}),
      default: MapSet.new()
    )
  end

  def init(args) do
    {:ok, %Communicator{engine: args[:name]}}
  end

  def start_link(arg) do
    GenServer.start_link(
      __MODULE__,
      arg,
      Utility.name(arg, &Utility.com_name/1)
    )
  end

  defdelegate record(engine, key, value), to: Engine

  def subscribe(communicator, subscriber, key) do
    GenServer.cast(communicator, {:subscribe, subscriber, key})
  end

  def handle_cast({:subscriber, subscriber, key}, agent) do
    {:noreply,
     %Communicator{
       agent
       | subscribers: MapSet.put(agent.subscribers, {subscriber, key})
     }}
  end

  def handle_cast({:record, key, value}, agent) do
    rel_subs = Control.handle_subs(agent.subscribers, key)

    Utility.broadcast(
      rel_subs,
      {:measurement_changed, key, value, Time.utc_now()}
    )

    {:noreply, agent}
  end
end
