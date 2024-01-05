defmodule Anoma.Node.Executor.Communicator do
  @moduledoc """
  I Manage the Pub Sub behavior

  If new intents come in, I send it to all my subscribers.

  Further I have the job of communicating to the `Primary` node when I
  get information for it to process.

  The information that is communicated is:

  1. intent solutions
  2. new intents

  """
  alias __MODULE__
  use TypedStruct
  use GenServer
  alias Anoma.Node.Executor.Worker
  alias Anoma.Node.Utility

  typedstruct do
    field(:subscribers, MapSet.t(GenServer.server()), default: MapSet.new())
    field(:spawner, atom())
    field(:ambiant_env, Nock.t())
  end

  def init(args) do
    environment =
      Map.merge(%Nock{}, args |> Enum.into(%{}))
      |> Map.delete(:name)

    {:ok, %Communicator{spawner: args[:name], ambiant_env: environment}}
  end

  def start_link(arg) do
    GenServer.start_link(
      __MODULE__,
      arg,
      Utility.name(arg, &Utility.com_name/1)
    )
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  # Transactions
  @doc """
  Spawns a new transaction, the returned task has the owner of the
  person sending in the message
  """
  @spec new_transaction(GenServer.server(), Noun.t(), Noun.t()) :: Task.t()
  def new_transaction(communicator, order, gate) do
    state = state(communicator)
    spawn_transactions(order, gate, state)
  end

  @spec new_transaction(GenServer.server(), Noun.t(), Noun.t(), Nock.t()) ::
          Task.t()
  def new_transaction(communicator, order, gate, env) do
    state = state(communicator)
    spawn_transactions(order, gate, %Communicator{state | ambiant_env: env})
  end

  @doc """

  Acts like `new_transaction/3`, but the caller does not care about
  the response. Instead the response is handled by the pool.

  The user gets back a process so it may keep track of sending
  messages to this task or terminating the task

  """
  @spec fire_new_transaction(GenServer.server(), Noun.t(), Noun.t()) :: pid()
  def fire_new_transaction(communicator, order, gate) do
    GenServer.call(communicator, {:transaction, order, gate})
  end

  @spec fire_new_transaction(GenServer.server(), Noun.t(), Noun.t(), Nock.t()) ::
          pid()
  def fire_new_transaction(communicator, order, gate, env) do
    GenServer.call(communicator, {:transaction, order, gate, env})
  end

  def snapshot(communicator) do
    GenServer.call(communicator, :snapshot)
  end

  def state(communicator) do
    GenServer.call(communicator, :state)
  end

  @spec subscribe(GenServer.server(), GenServer.server()) :: :ok
  def subscribe(communicator, subscriber) do
    GenServer.cast(communicator, {:subscribe, subscriber})
  end

  @spec reset(GenServer.server()) :: :ok
  def reset(communicator) do
    GenServer.cast(communicator, :reset)
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_call(:state, _from, state) do
    {:reply, state, state}
  end

  def handle_call(:snapshot, _from, state) do
    {:reply, hd(state.ambiant_env.snapshot_path), state}
  end

  def handle_call({:transaction, order, gate}, _from, state) do
    task = spawn_transactions(order, gate, state)
    {:reply, task.pid, state}
  end

  def handle_call({:transaction, order, gate, env}, _from, state) do
    task =
      spawn_transactions(order, gate, %Communicator{state | ambiant_env: env})

    {:reply, task.pid, state}
  end

  def handle_cast({:subscribe, new_sub}, agent) do
    subscribers = MapSet.put(agent.subscribers, new_sub)
    {:noreply, %Communicator{agent | subscribers: subscribers}}
  end

  def handle_cast(:reset, agent) do
    {:noreply, %Communicator{agent | subscribers: MapSet.new()}}
  end

  # TODO communicate this information somehow
  def handle_info({:DOWN, _ref, :process, pid, _reason}, state) do
    Utility.broadcast(state.subscribers, {:process_done, pid})
    {:noreply, state}
  end

  def handle_info(process, state) do
    Utility.broadcast(state.subscribers, {:process_done, process})
    {:noreply, state}
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  # make this more interesting later
  @spec spawn_transactions(Noun.t(), Noun.t(), t()) :: Task.t()
  defp spawn_transactions(order, gate, state) do
    Task.Supervisor.async(state.spawner, Worker, :run, [
      order,
      gate,
      state.ambiant_env
    ])
  end
end
