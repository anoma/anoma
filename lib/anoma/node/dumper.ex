defmodule Anoma.Node.Dumper do
  @moduledoc """
  I am the Dumper Engine. My role is to spawn asyncronous processes which
  listen to a specified block table announcement and await when blocks
  modulo my count have been executed, at which point I checkpoint the
  assigned node using the duping system using a file with a specified name
  """
  alias __MODULE__
  alias Anoma.Node.Router
  alias Anoma.Node.Logger
  use Router.Engine

  use TypedStruct

  typedstruct do
    field(:count, non_neg_integer(), enforce: false)
    field(:configuration, Router.Addr.t())
    field(:task, Task.t(), enforce: false)
    field(:logger, Router.Addr.t(), enforce: false)
  end

  def init(%Dumper{} = state) do
    {:ok, state}
  end

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
  I start a dumping loop based on the count supplied. If an old task was
  present before the start, I shut it down. If the count is nil I return
  a state with nil count and nil task.
  """
  def start(server) do
    Router.call(server, :start)
  end

  @doc """
  I shutdown the dumper task registered in the struct.
  """
  def stop(server) do
    Router.call(server, :stop)
  end

  @doc """
  I change the count of a given dumper engine. I first shutdown the current
  task, start a new task with a changed count parameter and return and
  register both the count and the task changes.

  If count is a non-positive integer, I do nothing. If it is nil, I set both
  the count and the task at hand to nil.
  """
  def set_count(server, count) do
    Router.cast(server, {:set, count})
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_call(:start, _from, state) do
    {msg, st} = case_on_count(state.count, state)
    {:reply, msg, st}
  end

  def handle_call(:stop, _from, state) do
    logger = state.logger

    task_shutdown_log(state.task, logger)

    log_info({:stop, logger})
    {:reply, :ok, %Dumper{state | task: nil}}
  end

  def handle_cast({:set, count}, _from, state) do
    {_msg, st} = case_on_count(count, state)
    {:noreply, st}
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

        {"Nil count specified", %Dumper{state | count: nil, task: nil}}
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
          {:ok, %Dumper{state | task: task}}
        else
          {"Bad input", state}
        end
      end
    else
      {"No config specified", state}
    end
  end

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

  ############################################################
  #                     Conceptual Helpers                   #
  ############################################################

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
    Logger.add(logger, :debug, "Task shut down: #{inspect(task)}")
  end

  defp log_info({:shutting_down_reply, task, reply, logger}) do
    Logger.add(
      logger,
      :debug,
      "Task shutting down with reply. Task: #{inspect(task)}. Reply: #{inspect(reply)}"
    )
  end

  defp log_info({:shutting_down_exit, task, reason, logger}) do
    Logger.add(
      logger,
      :error,
      "Task shutting down with exit flag. Task: #{inspect(task)}. Reason: #{inspect(reason)}"
    )
  end

  defp log_info({:new_task, task, logger}) do
    Logger.add(logger, :debug, "New task created. Task: #{inspect(task)}")
  end

  defp log_info({:stop, logger}) do
    Logger.add(logger, :debug, "Stopping the dumper")
  end

  defp log_info({:dump, logger}) do
    Logger.add(
      logger,
      :debug,
      "Dumping succesful from worker."
    )
  end
end
