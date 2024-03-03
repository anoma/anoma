defmodule Anoma.Node.Solver.Communicator do
  use GenServer
  use TypedStruct
  alias __MODULE__
  alias Anoma.Node.Utility
  alias Anoma.Node.Solver.Solver
  alias Anoma.Node.Logger

  typedstruct do
    field(:subscribers, MapSet.t(GenServer.server()), default: MapSet.new())
    field(:solver, atom(), require: true)
    field(:logger, atom(), enforce: false)
  end

  def init(name: name, logger: logger) do
    {:ok,
     %Communicator{
       solver: name,
       logger: logger
     }}
  end

  def start_link(arg) do
    GenServer.start_link(
      __MODULE__,
      arg,
      Utility.name(arg, &Utility.com_name/1)
    )
  end

  @spec add_intent(GenServer.server(), Anoma.Resource.Transaction.t()) :: :ok
  def add_intent(communicator, intent) do
    GenServer.cast(communicator, {:add_intent, intent})
  end

  @spec del_intents(GenServer.server(), list(Anoma.Resource.Transaction.t())) ::
          :ok
  def del_intents(communicator, intents) do
    GenServer.cast(communicator, {:del_intents, intents})
  end

  @spec get_solved(GenServer.server()) :: list(Anoma.Resource.Transaction.t())
  def get_solved(communicator) do
    GenServer.call(communicator, :get_solved)
  end

  @spec subscribe(GenServer.server(), GenServer.server(), boolean()) :: :ok
  def subscribe(communicator, subscriber, want_old) do
    GenServer.cast(communicator, {:subscribe, subscriber, want_old})
  end

  def handle_cast({:add_intent, intent}, com) do
    solver = com.solver
    subs = com.subscribers
    logger = com.logger
    solutions = Solver.add_intent(solver, intent)
    log_info({:add, solver, com.logger})

    # Be nice and don't tell anyone about empty solutions
    if solutions != [] do
      log_info({:solution_cast, subs, logger})

      Utility.broadcast(
        subs,
        {:solutions, solutions}
      )
    end

    {:noreply, com}
  end

  def handle_cast({:del_intents, intents}, com) do
    subs = com.subscribers
    solver = com.solver
    logger = com.logger
    log_info({:del, solver, logger})
    solutions = Solver.del_intents(solver, intents)

    if solutions != [] do
      log_info({:solution_cast, subs, logger})

      Utility.broadcast(
        subs,
        {:solutions, solutions}
      )
    end

    {:noreply, com}
  end

  def handle_cast({:subscribe, new_sub, want_old}, com) do
    if want_old do
      log_info({:solution_cast, new_sub, com.logger})

      Utility.broadcast(
        [new_sub],
        {:solutions, Solver.get_solved(com.solver)}
      )
    end

    {:noreply,
     %Communicator{com | subscribers: MapSet.put(com.subscribers, new_sub)}}
  end

  def handle_call(:get_solved, _from, com) do
    solver = com.solver
    log_info({:get, solver, com.logger})

    {:reply, Solver.get_solved(solver), com}
  end

  ############################################################
  #                     Logging Info                         #
  ############################################################

  defp log_info({:add, solver, logger}) do
    Logger.add(
      logger,
      self(),
      "Requested to add intent from: #{inspect(solver)}"
    )
  end

  defp log_info({:solution_cast, subs, logger}) do
    Logger.add(logger, self(), "Broadcast solutions to: #{inspect(subs)}")
  end

  defp log_info({:del, solver, logger}) do
    Logger.add(
      logger,
      self(),
      "Requested to delete intent from: #{inspect(solver)}"
    )
  end

  defp log_info({:get, solver, logger}) do
    Logger.add(logger, self(), "Requested solution from: #{inspect(solver)}")
  end
end
