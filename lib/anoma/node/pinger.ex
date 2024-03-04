defmodule Anoma.Node.Pinger do
  @moduledoc """
  I provide periodic block execution based on submitted mempool name and time.
  """
  use GenServer
  use TypedStruct

  alias Anoma.Node.Router
  alias Anoma.Node.Mempool
  alias __MODULE__

  typedstruct do
    field(:mempool, Router.Addr.t())
    field(:time, non_neg_integer() | atom(), default: :no_timer)
  end

  def init(args) do
    time = args[:time]
    mempool = args[:mempool]

    pinger(time)
    {:ok, %Pinger{mempool: mempool, time: time}}
  end

  def handle_info(:execute, state) do
    Mempool.execute(state.mempool)
    pinger(state.time)

    {:noreply, state}
  end

  @doc """
  I send the :execute message after specified time if ever.
  """
  def pinger(time) do
    if time == :no_timer do
      :ok
    else
      Process.send_after(self(), :execute, time)
    end
  end
end
