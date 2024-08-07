defmodule Anoma.Node.Pinger do
  @moduledoc """
  I am the Pinger Engine.

  I provide periodic block execution based on submitted mempool address
  and time by calling the execution API in the mempool engine.

  ### Public APIs
  I have the following public functionality:

  - `start/1`
  - `pinger/1`
  - `set_timer/2`
  """
  alias Anoma.Node.Router
  alias Anoma.Node.Logger
  alias Anoma.Node.Mempool
  alias __MODULE__

  use TypedStruct
  use Router.Engine

  typedstruct do
    @typedoc """
    I am the type of the Pinger Engine instance.

    I store minimal info required to ask the mempool to execute, namely the
    mempool address and the time specified by the user.

    - `:mempool` - The Mempool Engine address which is called to execute.
    - `:time` - The time that should be elapsed between the calls to
                execute or an atom saying that no timer should be set.
                Default: `:no_timer`
    - `:logger` - The address of the Logger Engine.
    """
    field(:mempool, Router.Addr.t())
    field(:time, non_neg_integer() | atom(), default: :no_timer)
    field(:logger, Router.Addr.t())
  end

  @doc """
  I am the initialiation function for the Pinger Engine.

  ### Patern-Matching Variations

  - `init(%Pinger{})` - I initialize the Engine with the given state.
  - `init(args)` - I expect a keylist with the `:mempool`, `:time` and
                   `:logger` keywords and then start a Pinger Engine
                   instance with appropriate state.
  """

  @spec init(Pinger.t()) :: {:ok, Pinger.t()}
  def init(%Pinger{} = state) do
    pinger(state.time)
    {:ok, state}
  end

  @spec init(
          list(
            {:mempool, Router.Addr.t()}
            | {:time, non_neg_integer()}
            | {:logger, Router.Addr.t()}
          )
        ) ::
          {:ok, Pinger.t()}
  def init(args) do
    time = args[:time]
    mempool = args[:mempool]
    logger = args[:logger]

    {:ok, %Pinger{mempool: mempool, time: time, logger: logger}}
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @doc """
  I set the timer field for a Pinger Engine instance.

  Given a server S and time T I change the timer set for the struct
  connected to S setting it to T. Set T to :no_timer to stop the
  pinger.
  """

  @spec set_timer(Router.Addr.t(), non_neg_integer() | :no_timer) :: :ok
  def set_timer(server, time) do
    Router.cast(server, {:set, time})
  end

  @doc """
  I start the Pinger Engine's execution-calling functionality.

  Given a pinger address, I start up the pinger by calling the `pinger/1`
  function feeding it the time associated to the address.

  Note that if the timer specified has value :no_timer, the pinger will
  not practically start.
  """

  @spec start(Router.Addr.t()) :: :ok
  def start(server) do
    Router.cast(server, :start)
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_cast(:start, _from, state) do
    pinger(state.time)
    log_info({:launch, state.logger})
    {:noreply, state}
  end

  def handle_cast({:set, time}, _from, state) do
    log_info({:set, time, state.logger})
    {:noreply, %Pinger{state | time: time}}
  end

  def handle_info(:execute, state) do
    Mempool.execute(state.mempool)
    pinger(state.time)

    {:noreply, state}
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  @doc """
  I am the pinger function.

  I receive an argument which is either an integer or :no_timer.
  If it is an integer, send to self an :execute message after
  the specified amount of time. Otherwise, simply reply :ok
  """

  @spec pinger(:no_timer | non_neg_integer()) :: :ok | reference()
  def pinger(time) do
    if time == :no_timer do
      :ok
    else
      Process.send_after(self(), :execute, time)
    end
  end

  ############################################################
  #                     Logging Info                         #
  ############################################################

  defp log_info({:set, time, logger}) do
    Logger.add(logger, :info, "Timer set to #{inspect(time)}")
  end

  defp log_info({:launch, logger}) do
    Logger.add(logger, :info, "Pinger launched")
  end
end
