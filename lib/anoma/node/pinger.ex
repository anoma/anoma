defmodule Anoma.Node.Pinger do
  @moduledoc """
  I provide periodic block execution based on submitted mempool address
  and time by calling the execution API in the mempool engine.

  ### Public APIs
  My public functionality include:

  - `set_timer/2`
  - `start/1`
  - `pinger/1`
  """
  alias Anoma.Node.Router
  alias Anoma.Node.Mempool
  alias __MODULE__

  use TypedStruct
  use Router.Engine

  typedstruct do
    field(:mempool, Router.Addr.t())
    field(:time, non_neg_integer() | atom(), default: :no_timer)
  end

  def init(%Pinger{} = state) do
    pinger(state.time)

    {:ok, state}
  end

  @spec init(list({:mempool, Router.Addr.t()} | {:time, non_neg_integer()})) ::
          {:ok, Pinger.t()}
  def init(args) do
    time = args[:time]
    mempool = args[:mempool]

    {:ok, %Pinger{mempool: mempool, time: time}}
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @doc """
  Given a server S and time T I change the timer set for the struct
  connected to S setting it to T. Set T to :no_timer to stop the
  pinger.
  """

  @spec set_timer(Router.Addr.t(), non_neg_integer() | :no_timer) ::
          String.t()
  def set_timer(server, time) do
    Router.call(server, {:set, time})
  end

  @doc """
  Given a pinger address, I start up the pinger by calling the `pinger/1`
  function feeding it the time associated to the address.

  Note that if the timer specified has value :no_timer, the pinger will
  not practically start.
  """

  @spec start(Router.Addr.t()) :: String.t()
  def start(server) do
    Router.call(server, :start)
  end

  def state(server) do
    Router.call(server, :state)
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_call(:state, _from, state) do
    {:reply, state, state}
  end

  def handle_call(:start, _from, state) do
    pinger(state.time)

    {:reply, "Pinger launched", state}
  end

  def handle_call({:set, time}, _from, state) do
    {:reply, "Timer set to #{inspect(time)}", %Pinger{state | time: time}}
  end

  def handle_info(:execute, state) do
    Mempool.execute(state.mempool)
    pinger(state.time)

    {:noreply, state}
  end

  ############################################################
  #                          Helpers                         #
  ############################################################

  @doc """
  I receive an argument which is either an integer or :no_timer.
  If it is an integer, send to self an :execute message after
  specified ammount of time. Otherwise, simply reply :ok
  """

  @spec pinger(:no_timer | non_neg_integer()) :: :ok | reference()
  def pinger(time) do
    if time == :no_timer do
      :ok
    else
      Process.send_after(self(), :execute, time)
    end
  end
end
