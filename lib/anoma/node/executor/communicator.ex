defmodule Anoma.Node.Executor do
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

  alias Anoma.Node.Executor.Worker
  alias Anoma.Node.Router
  alias Anoma.Node.Logger

  typedstruct do
    field(:task_completion_topic, Router.Addr.t())
    field(:ambiant_env, Nock.t())
    field(:tasks, list(Task.t()), default: [])
    field(:logger, Router.Addr.t(), enforce: false)
  end

  def init({env, topic, logger}) do
    {:ok,
     %Executor{ambiant_env: env, task_completion_topic: topic, logger: logger}}
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  # Transactions
  @doc """
  Spawns a new transaction, the returned task's owner is the caller.

  ### Inputs
    - `executor` - the genserver to send the message to
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
      > task = Executor.new_transaction(executor, id, {:kv, zero})
      %Task{
        mfa: {Anoma.Node.Executor.Worker, :run, 3},
        owner: #PID<0.264.0>,
        pid: #PID<0.753.0>,
        ref: #Reference<0.0.33795.904691850.4087414796.24378>
      }
  """
  @spec new_transaction(Router.Addr.t(), Noun.t(), Noun.t()) :: Task.t()
  def new_transaction(executor, order, gate) do
    state = state(executor)
    spawn_transactions(order, gate, state)
  end

  @spec new_transaction(Router.Addr.t(), Noun.t(), Noun.t(), Nock.t()) ::
          Task.t()
  def new_transaction(executor, order, gate, env) do
    state = state(executor)
    spawn_transactions(order, gate, %Executor{state | ambiant_env: env})
  end

  @doc """
  Acts like `new_transaction/3`, but the caller does not care about
  the response. Instead the response is handled by the pool.

  The user gets back a process so it may keep track of sending
  messages to this task or terminating the task
  """
  @spec fire_new_transaction(
          Router.Addr.t(),
          Noun.t(),
          Transaction.execution()
        ) :: pid()
  def fire_new_transaction(executor, order, gate) do
    Router.call(executor, {:transaction, order, gate})
  end

  @spec fire_new_transaction(Router.Addr.t(), Noun.t(), Noun.t(), Nock.t()) ::
          pid()
  def fire_new_transaction(executor, order, gate, env) do
    Router.call(executor, {:transaction, order, gate, env})
  end

  @doc """
  Returns the snapshot path, the Nock code is using

  ### Example
    iex> alias Anoma.Node.{Executor, Router}
    iex> {:ok, router} = Router.start
    iex> snap = %{snapshot_path: [:a | 0], ordering: nil}
    iex> args = {snap, Router.new_topic(router), nil}
    iex> {:ok, addr} = Router.start_engine(router, Executor, args)
    iex> Executor.snapshot(addr)
    :a
  """
  def snapshot(executor) do
    Router.call(executor, :snapshot)
  end

  @doc """
  Returns the current state of the executor
  """
  def state(executor) do
    Router.call(executor, :state)
  end

  # TODO, only kill the given transactions
  def kill_transactions(communicator, _trans) do
    Router.call(communicator, :kill)
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_call(:state, _from, state) do
    log_info({:state, state, state.logger})
    {:reply, state, state}
  end

  def handle_call(:snapshot, _from, state) do
    hd = hd(state.ambiant_env.snapshot_path)
    log_info({:snap, hd, state.logger})
    {:reply, hd, state}
  end

  def handle_call({:transaction, order, gate}, _from, state) do
    logger = state.logger

    log_info({:tx_call, logger})
    task = spawn_transactions(order, gate, state)
    log_info({:tx_call_pid, task.pid, logger})
    {:reply, task.pid, %__MODULE__{state | tasks: [task | state.tasks]}}
  end

  def handle_call({:transaction, order, gate, env}, _from, state) do
    logger = state.logger

    log_info({:tx_call_env, env, logger})

    task =
      spawn_transactions(order, gate, %Executor{state | ambiant_env: env})

    log_info({:tx_call_pid, task.pid, logger})

    {:reply, task.pid, %__MODULE__{state | tasks: [task | state.tasks]}}
  end

  def handle_call(:kill, _from, agent) do
    kill(agent.tasks)
    {:reply, :ok, %__MODULE__{agent | tasks: []}}
  end

  def handle_cast(:kill, agent) do
    kill(agent.tasks)
    {:reply, :ok, %__MODULE__{agent | tasks: []}}
  end

  def handle_cast(:reset, agent) do
    log_info({:reset_sub, agent.logger})
    {:noreply, %__MODULE__{agent | tasks: []}}
  end

  def handle_info({:DOWN, _ref, :process, pid, _reason}, state) do
    log_info({:cast_down, state.logger})
    Router.cast(state.task_completion_topic, {:process_done, pid})
    {:noreply, state}
  end

  def handle_info(process, state) do
    log_info({:cast_process, state.logger})
    Router.cast(state.task_completion_topic, {:process_done, process})
    {:noreply, state}
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  # make this more interesting later
  @spec spawn_transactions(Noun.t(), Noun.t(), t()) :: Task.t()
  defp spawn_transactions(order, gate, state) do
    log_info({:spawn, order, state.logger})

    Task.async(Worker, :run, [
      order,
      gate,
      state.ambiant_env
    ])
  end

  @spec kill(Task.t()) :: :ok
  defp kill(tasks) do
    Enum.each(tasks, &Task.shutdown/1)
    :ok
  end

  ############################################################
  #                     Logging Info                         #
  ############################################################

  defp log_info({:state, state, logger}) do
    Logger.add(logger, :info, "Requested state: #{inspect(state)}")
  end

  defp log_info({:snap, hd, logger}) do
    Logger.add(logger, :info, "Requested snapshot: #{inspect(hd)}")
  end

  defp log_info({:tx_call, logger}) do
    Logger.add(logger, :info, "Requested to spawn transaction")
  end

  defp log_info({:tx_call_pid, pid, logger}) do
    Logger.add(
      logger,
      :info,
      "Spawned transaction. PID: #{inspect(pid)}"
    )
  end

  defp log_info({:tx_call_env, env, logger}) do
    Logger.add(
      logger,
      :info,
      "Requested to spawn transaction in environment: #{inspect(env)}"
    )
  end

  defp log_info({:reset_sub, logger}) do
    Logger.add(logger, :debug, "Requested subscribers reset")
  end

  defp log_info({:cast_down, logger}) do
    Logger.add(logger, :debug, "Broadcasting process with tag :DOWN.")
  end

  defp log_info({:cast_process, logger}) do
    Logger.add(logger, :info, "Broadcasting process.")
  end

  defp log_info({:spawn, order, logger}) do
    Logger.add(
      logger,
      :info,
      "Spawning worker with order: #{inspect(order)}"
    )
  end
end
