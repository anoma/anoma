defmodule Anoma.Node.Executor do
  @moduledoc """
  I am the Executor Engine.

  My main responsibility is to be the intemediary between the Mempool and
  transaction spawning by launching the Worker Engines provided the pending
  transaction code.

  ### API

  My public facing API is

  - `snapshot/1`
  - `kill_transactions/2`
  - `fire_new_transaction/4`
  """

  alias __MODULE__
  alias Anoma.Node.Executor.Worker
  alias Anoma.Node.Router
  alias Anoma.Node.Logger

  use TypedStruct
  use Router.Engine

  typedstruct do
    @typedoc """
    I am the type of the Executor Engine.

    ### Fields

    - `:router` - The Router Engine address used by the Executor.
    - `:topic` - The topic for broadcasting.
    - `:ambiant_env` - The default environment to be fed to spawned workers.
    - `:workers` - A list of worker addresses spawned.
                   Default: []
    - `:logger` - The Logger Engine address.
                  Enforced: false
    """

    field(:router, Router.Addr.t())
    field(:topic, Router.Addr.t())
    field(:ambiant_env, Nock.t())
    field(:workers, list(Router.Addr.t()), default: [])
    field(:logger, Router.Addr.t(), enforce: false)
  end

  @doc """

   ### Pattern-Matching Variations

  - `init(%Executor{})` - I initialize the Engine with the given state.
  - `init({router, env, topic, logger})` - I expect a tuple with needed
                                           arguments to launch the executor.
  """
  @spec init(Executor.t()) :: {:ok, Executor.t()}
  def init(%Executor{} = state) do
    {:ok, state}
  end

  @spec init({Router.Addr.t(), Nock.t(), Router.Addr.t(), Router.Addr.t()}) ::
          {:ok, Executor.t()}
  def init({router, env, topic, logger}) do
    {:ok,
     %Executor{
       router: router,
       ambiant_env: %Nock{env | logger: logger},
       topic: topic,
       logger: logger
     }}
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @doc """
  I am a function spawning a new transaction.

  I do so by spawning a new engine with the info provided. If the
  environment is left unspecified I launch the Worker with the given
  ambient environment stored in the state.
  The computed value will be sent to the specified reply-to address.
  """
  @spec fire_new_transaction(
          Router.Addr.t(),
          non_neg_integer(),
          Noun.t(),
          Nock.t() | nil,
          Router.addr() | nil
        ) ::
          :ok
  def fire_new_transaction(executor, id, gate, env \\ nil, reply_to) do
    Router.cast(executor, {:transaction, id, gate, env, reply_to})
  end

  @doc """
  I am a function killing transactions.

  I currently do not filter through which transactions to kill. I just go
  through the list of all Workers stored in my state and kill their
  processes one by one. The returned state will have a nil list for the
  current worker list.
  """

  @spec kill_transactions(Router.Addr.t(), list(Anoma.Transaction.t())) :: :ok
  def kill_transactions(executor, transactions) do
    Router.cast(executor, {:kill, transactions})
  end

  @doc """
  I return the snapshot path.
  """

  @spec snapshot(Router.Addr.t()) :: atom()
  def snapshot(executor) do
    Router.call(executor, :snapshot)
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_call(:snapshot, _from, state) do
    hd = hd(state.ambiant_env.snapshot_path)
    log({:snap, hd, state.logger})
    {:reply, hd, state}
  end

  def handle_cast({:transaction, order, gate, env, reply_to}, _from, state) do
    logger = state.logger

    worker_addr =
      unless env do
        spawn_transactions(order, gate, state, reply_to)
      else
        spawn_transactions(
          order,
          gate,
          %__MODULE__{state | ambiant_env: env},
          reply_to
        )
      end

    log({:tx_call_addr, worker_addr, logger})

    Router.cast(state.topic, {:worker_spawned, worker_addr})

    {:noreply, %__MODULE__{state | workers: [worker_addr | state.workers]}}
  end

  def handle_cast({:kill, _transactions}, _from, state) do
    kill(state.workers)
    {:noreply, %__MODULE__{state | workers: []}}
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  # make this more interesting later
  @spec spawn_transactions(Noun.t(), Noun.t(), t(), Router.addr() | nil) ::
          Router.addr()
  defp spawn_transactions(order, gate, state, reply_to) do
    log({:spawn, order, state.logger})

    {:ok, addr} =
      Router.start_engine(
        state.router,
        Worker,
        {order, gate, state.ambiant_env, state.topic, reply_to}
      )

    addr
  end

  @spec kill([Router.Addr.t()]) :: :ok
  defp kill(workers) do
    # some tasks will have no pids because they have already terminated; cope

    workers
    |> Enum.map(&Router.Addr.pid/1)
    |> Enum.reject(&is_nil/1)
    |> Enum.each(&Process.exit(&1, :kill))

    :ok
  end

  ############################################################
  #                     Logging Info                         #
  ############################################################

  defp log({:snap, hd, logger}) do
    Logger.add(logger, :info, "Requested snapshot: #{inspect(hd)}")
  end

  defp log({:tx_call_addr, pid, logger}) do
    Logger.add(
      logger,
      :info,
      "Spawned transaction. Address: #{inspect(pid)}"
    )
  end

  defp log({:spawn, order, logger}) do
    Logger.add(
      logger,
      :info,
      "Spawning worker with order: #{inspect(order)}"
    )
  end
end
