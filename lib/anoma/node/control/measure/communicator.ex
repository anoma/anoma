defmodule Anoma.Node.Control.Measure.Communicator do
  alias __MODULE__

  use TypedStruct
  use GenServer

  alias Anoma.Node.Control.Measure.Engine
  alias Anoma.Node.Utility

  typedstruct do
    field(:engine, GenServer.server(), enforce: true)

    field(:subscribers, Enum.t(), default: %{})
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
       | subscribers:
           Map.update(
             agent.subscribers,
             key,
             MapSet.new([subscriber]),
             fn set -> MapSet.put(set, subscriber) end
           )
     }}
  end

  def handle_cast({:record, key, value}, agent) do
    Utility.broadcast(
      Map.get(agent.subscribers, key),
      {:measurement_changed, key, value, Time.utc_now()}
    )

    {:noreply, agent}
  end
end
