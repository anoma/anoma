defmodule Anoma.Node.Pinger do
  @moduledoc """
  I provide automatic block execution every 10 seconds in the production environment.
  """
  use GenServer

  alias Anoma.Node.Mempool.Communicator, as: Mcom

  def start_link(env) do
    Mcom.hard_reset(:anoma_mempool_com)
    GenServer.start_link(__MODULE__, env)
  end

  def init(env) do
    if env == :prod do
      pinger()
    end

    {:ok, env}
  end

  def handle_info(:execute, state) do
    Mcom.execute(:anoma_mempool_com)
    pinger()

    {:noreply, state}
  end

  @doc """
  I send the :execute message every 10 seconds.
  """
  def pinger() do
    Process.send_after(self(), :execute, 10000)
  end
end
