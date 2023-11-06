defmodule Anoma.Node.Ordering do
  @moduledoc """
  Dummy ordering service.
  """

  use GenServer

  def start_link() do
    GenServer.start_link(__MODULE__, nil, name: __MODULE__)
  end

  def init(_opts) do
    {:ok, 1}
  end

  def handle_call(_msg, _from, state) do
    {:reply, :ok, state}
  end

  def handle_cast({:tx, tx_code}, state) do
    spawn(Anoma.Node.Worker, :run, [state - 1, tx_code])
    {:noreply, state + 1}
  end

  def handle_cast({:inject, order, tx_code}, state) do
    spawn(Anoma.Node.Worker, :run, [order - 1, tx_code])
    {:noreply, state}
  end

  def handle_cast(:reset, _state) do
    {:noreply, 1}
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end

  def tx(tx_code) do
    GenServer.cast(__MODULE__, {:tx, tx_code})
  end

  def inject_tx(order, tx_code) do
    GenServer.cast(__MODULE__, {:inject, order, tx_code})
  end

  def reset() do
    GenServer.cast(__MODULE__, :reset)
  end
end
