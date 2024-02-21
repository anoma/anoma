defmodule Anoma.Node.Pinger do
  @moduledoc """
  I provide automatic block execution every 10 seconds in the production environment.
  """
  use GenServer

  alias Anoma.Node.Mempool.Communicator, as: Mcom

  def start_link(args) do
    Mcom.hard_reset(args[:name])
    GenServer.start_link(__MODULE__, args)
  end

  def init(args) do
    if args[:environment] == :prod do
      pinger(args[:name])
    end

    {:ok, args}
  end

  def handle_info({:execute, mempool}, state) do
    Mcom.execute(mempool)
    pinger(mempool)

    {:noreply, state}
  end

  @doc """
  I send the :execute message every 10 seconds.
  """
  def pinger(mempool) do
    Process.send_after(self(), {:execute, mempool}, 10000)
  end
end
