defmodule Anoma.Node.Control.Measure.Engine do
  use GenServer
  use TypedStruct

  alias __MODULE__
  alias Anoma.Storage
  alias Anoma.Node.Utility

  typedstruct do
    field(:storage, Storage.t())
  end

  def init(arg) do
    {:ok, %Engine{storage: arg[:name]}}
  end

  def start_link(arg) do
    GenServer.start_link(__MODULE__, arg, Utility.name(arg))
  end

  def record(engine, key, value) do
    GenServer.cast(engine, {:record, key, value})
  end

  def handle_cast({:record, key, value}, engine) do
    with {:ok, list} <- Storage.get(engine.storage, key) do
      Storage.put(engine.storage, measure_space(key), [
        timestamp(value) | list
      ])
    else
      :absent ->
        Storage.put(engine.storage, measure_space(key), [timestamp(value)])
    end

    {:noreply, engine}
  end

  def timestamp(meas) do
    {meas, Time.utc_now()}
  end

  defp measure_space(key) do
    [:measurement, key]
  end
end
