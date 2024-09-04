defmodule Anoma.Node.Dumper do
  @moduledoc """
  I am the Dumper Engine.

  My role is to spawn asynchronous processes which
  listen to a specified block table announcement and await when blocks
  modulo my count have been executed, at which point I checkpoint the
  assigned node using the duping system using a file with a specified name.

  ### Public API
  I have the following public functionality:

  - `start/1`
  - `stop/1`
  - `set_count/2`
  """

  alias __MODULE__
  alias Anoma.Node.Router
  alias Anoma.Node.EventLogger

  use Router.Engine
  use TypedStruct

  typedstruct do
    @typedoc """
    I am the type of the Dumper Engine.

    I store the number of blocks to be passed before snapshotting, the
    Configuration Engine address, as well as the task responsible for the
    snapshotting.

    ### Fields

    - `:count` - Field specifying how many blocks ought to pass between
                 snapshots. Enforced: false.
    - `:configuration` - The Configuration Engine address using which the
                         snapshot path is found.
    - `:task` - The field containing the task carrying out the
                snapshotting. Enforced: false.
    - `:logger` - The address of the Logger Engine. Enforced: false.
    """

    field(:count, non_neg_integer(), enforce: false)
    field(:configuration, Router.Addr.t())
    field(:task, Task.t(), enforce: false)
    field(:logger, Router.Addr.t(), enforce: false)
  end

  @doc """
  I am the initialization function for the Dumper Engine.

  ### Pattern-Matching Variations

  - `init(%Dumper{})` - I initialize the Engine with given state.
  - `init(opts)` - I expect a keylist with the `:count`, `:configuration`,
                   and `:logger` keys available. I then start the engine
                   instance with appropriate state.
  """

  @spec init(Dumper.t()) :: {:ok, Dumper.t()}
  def init(%Dumper{} = state) do
    {:ok, state}
  end

  @spec init(
          list(
            {:count, non_neg_integer | nil}
            | {:configuration, Router.Addr.t()}
            | {:logger, Router.Addr.t() | nil}
          )
        ) :: {:ok, Dumper.t()}
  def init(opts) do
    count = opts[:count]
    configuration = opts[:configuration]
    logger = opts[:logger]

    {:ok,
     %Dumper{
       count: count,
       configuration: configuration,
       logger: logger
     }}
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @doc """
  I am the Dumper start function.

  I start a dumping loop based on the count supplied. If an old task was
  present before the start, I shut it down. If the count is nil I return
  a state with nil count and nil task.
  """

  @spec start(Router.Addr.t()) :: :ok
  def start(server) do
    Router.cast(server, :start)
  end

  @doc """
  I am the Dumper stop function.

  I shutdown the dumper task registered in the struct. Depending on the
  shutdown message, I provide separate logs and then set the task field to
  `nil`.
  """

  @spec stop(Router.Addr.t()) :: :ok
  def stop(server) do
    Router.cast(server, :stop)
  end

  @doc """
  I set the count of a Dumper Engine instance.

  I first shutdown the current task, start a new task with a changed count
  parameter and return and register both the count and the task changes.

  If count is a non-positive integer, I do nothing. If it is nil, I set both
  the count and the task at hand to nil.
  """

  @spec set_count(Router.Addr.t(), non_neg_integer | nil) :: :ok
  def set_count(server, count) do
    Router.cast(server, {:set, count})
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_cast(:start, _from, state) do
    {:noreply, case_on_count(state.count, state)}
  end

  def handle_cast(:stop, _from, state) do
    logger = state.logger

    task_shutdown_log(state.task, logger)

    log_info({:stop, logger})
    {:noreply, %Dumper{state | task: nil}}
  end

  def handle_cast({:set, count}, _from, state) do
    {:noreply, case_on_count(count, state)}
  end

  def handle_info({:DOWN, _ref, :process, _pid, _reason}, state) do
    {:noreply, state}
  end

  def handle_info(_process, state) do
    {:noreply, state}
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  @spec case_on_count(nil | non_neg_integer, Dumper.t()) :: Dumper.t()
  defp case_on_count(count, state) do
    config =
      Anoma.Node.Router.Engine.get_state(state.configuration).configuration

    if config != nil do
      logger = state.logger

      table =
        config["node"][
          "block_storage"
        ]
        |> String.to_atom()

      if count == nil do
        task_shutdown_log(state.task, logger)

        %Dumper{state | count: nil, task: nil}
      else
        if is_integer(count) and count > 0 do
          task_shutdown_log(state.task, logger)

          task =
            Task.async(fn ->
              :mnesia.subscribe({:table, table, :simple})

              dump_loop(
                count,
                table,
                state.configuration,
                state.logger
              )
            end)

          log_info({:new_task, task, logger})
          %Dumper{state | count: count, task: task}
        else
          state
        end
      end
    else
      state
    end
  end

  @spec dump_loop(non_neg_integer, atom(), Router.Addr.t(), Router.Addr.t()) ::
          any()
  defp dump_loop(count, block_table, config, logger) do
    receive do
      {:mnesia_table_event,
       {:write, {^block_table, _, _, n, _, _}, {:tid, _, _}}} ->
        # We start count from the zeroeth block
        if rem(n + 1, count) == 0 do
          Anoma.Node.Configuration.snapshot(config)
          log_info({:dump, logger})
        end

        dump_loop(count, block_table, config, logger)
    end
  end

  @spec task_shutdown_log(Task.t(), Router.Addr.t()) :: :ok
  defp task_shutdown_log(task, logger) do
    unless task == nil do
      case Task.shutdown(task) do
        nil ->
          log_info({:shutting_down, task, logger})

        {:ok, reply} ->
          log_info({:shutting_down_reply, task, reply, logger})

        {:exit, reason} ->
          log_info({:shutting_down_exit, task, reason, logger})
      end
    end
  end

  ############################################################
  #                     Logging Info                         #
  ############################################################

  defp log_info({:shutting_down, task, logger}) do
    EventLogger.add(logger, :debug, "Task shut down: #{inspect(task)}")
  end

  defp log_info({:shutting_down_reply, task, reply, logger}) do
    EventLogger.add(
      logger,
      :debug,
      "Task shutting down with reply. Task: #{inspect(task)}. Reply: #{inspect(reply)}"
    )
  end

  defp log_info({:shutting_down_exit, task, reason, logger}) do
    EventLogger.add(
      logger,
      :error,
      "Task shutting down with exit flag. Task: #{inspect(task)}. Reason: #{inspect(reason)}"
    )
  end

  defp log_info({:new_task, task, logger}) do
    EventLogger.add(
      logger,
      :debug,
      "New task created. Task: #{inspect(task)}"
    )
  end

  defp log_info({:stop, logger}) do
    EventLogger.add(logger, :debug, "Stopping the dumper")
  end

  defp log_info({:dump, logger}) do
    EventLogger.add(
      logger,
      :debug,
      "Dumping call succesful from worker."
    )
  end
end
