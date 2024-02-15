defmodule Anoma.Node.Executor.Communicator do
  @moduledoc """
  I Manage the Pub Sub behavior

  If new intents come in, I send it to all my subscribers.

  Further I have the job of spawning Executor tasks when new
  transactions come in

  Currently I only communicate:

  - When tasks are completed

  ### API

  My public facing API is

  - `new_transaction/3`
  - `new_transaction/4`
  - `fire_new_transaction/3`
  - `fire_new_transaction/4`
  - `snapshot/1`
  - `state/1`
  - `subscribe/2`
  """
  alias Anoma.Transaction
  alias __MODULE__
  use TypedStruct
  use Anoma.Communicator, sub_field: :subscribers
  alias Anoma.Communicator, as: ACom

  alias Anoma.Node.Executor.Worker
  alias Anoma.Node.Utility

  typedstruct do
    field(:subscribers, ACom.t(), default: ACom.new())
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
  Spawns a new transaction, the returned task's owner is the caller.

  ### Inputs
    - `communicator` - the genserver to send the message to
    - `order` - the unique ID of the transaction
    - `gate` - a Nock function that we will spawn a task for

  ### Output
    - a task that the caller owns

  ### Example
    we assume a running Worker, see `executor_test.exs` for a full
    example

      > id = System.unique_integer([:positive])
      1931
      > zero = [[1, 777 | 0], 0, Nock.stdlib_core()]
      > task = Communicator.new_transaction(executor, id, zero)
      %Task{
        mfa: {Anoma.Node.Executor.Worker, :run, 3},
        owner: #PID<0.264.0>,
        pid: #PID<0.753.0>,
        ref: #Reference<0.0.33795.904691850.4087414796.24378>
      }
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
  @spec fire_new_transaction(
          GenServer.server(),
          Noun.t(),
          Transaction.execution()
        ) :: pid()
  def fire_new_transaction(communicator, order, gate) do
    GenServer.call(communicator, {:transaction, order, gate})
  end

  @spec fire_new_transaction(GenServer.server(), Noun.t(), Noun.t(), Nock.t()) ::
          pid()
  def fire_new_transaction(communicator, order, gate, env) do
    GenServer.call(communicator, {:transaction, order, gate, env})
  end

  @doc """
  Returns the snapshot path, the Nock code is using

  ### Example
    iex> alias Anoma.Node.Executor
    iex> alias Anoma.Node.Executor.Communicator
    iex> Executor.start_link(snapshot_path: [:a | 0], ordering: nil, name: :snapshot_doc)
    iex> Communicator.snapshot(:snapshot_doc_com)
    :a
  """
  def snapshot(communicator) do
    GenServer.call(communicator, :snapshot)
  end

  @doc """
  Returns the current state of the communicator
  """
  def state(communicator) do
    GenServer.call(communicator, :state)
  end

  @doc """
  Resets the subscribers list
  """
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
