defmodule Anoma.Node.Solver.Communicator do
  use GenServer
  use TypedStruct
  alias __MODULE__
  alias Anoma.Node.Utility
  alias Anoma.Node.Solver.Solver

  typedstruct do
    field(:subscribers, MapSet.t(GenServer.server()), default: MapSet.new())
    field(:solver, atom(), require: true)
  end

  def init(name: name) do
    {:ok, %Communicator{solver: name}}
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
    solutions = Solver.add_intent(com.solver, intent)

    # Be nice and don't tell anyone about empty solutions
    if solutions != [] do
      Utility.broadcast(
        com.subscribers,
        {:solutions, solutions}
      )
    end

    {:noreply, com}
  end

  def handle_cast({:del_intents, intents}, com) do
    solutions = Solver.del_intents(com.solver, intents)

    if solutions != [] do
      Utility.broadcast(
        com.subscribers,
        {:solutions, solutions}
      )
    end

    {:noreply, com}
  end

  def handle_cast({:subscribe, new_sub, want_old}, com) do
    if want_old do
      Utility.broadcast(
        [new_sub],
        {:solutions, Solver.get_solved(com.solver)}
      )
    end

    {:noreply,
     %Communicator{com | subscribers: MapSet.put(com.subscribers, new_sub)}}
  end

  def handle_call(:get_solved, _from, com) do
    {:reply, Solver.get_solved(com.solver), com}
  end
end
