defmodule Examples.Solver do
  @moduledoc """
  I contain several examples on how to use the solver.
  """

  require ExUnit.Assertions
  import ExUnit.Assertions

  alias Anoma.Node.IntentPool
  alias Anoma.Node.Solver
  alias Anoma.RM.DumbIntent

  ############################################################
  #                  Initialization                          #
  ############################################################

  @doc """
  I start a new intent pool and solver, and return their process id.
  """
  def initialization() do
    # start the event broker application.
    Application.ensure_all_started(:event_broker)

    # start an intent pool
    intent_pool_name =
      Enum.shuffle(?a..?z)
      |> Enum.take(20)
      |> to_string
      |> String.to_atom()

    {:ok, intent_pool_pid} =
      GenServer.start_link(IntentPool, [], name: intent_pool_name)

    # start a solver
    solver_name =
      Enum.shuffle(?a..?z)
      |> Enum.take(20)
      |> to_string
      |> String.to_atom()

    intent_pool_args = [intent_pool: intent_pool_pid]

    {:ok, solver_pid} =
      GenServer.start_link(Solver, intent_pool_args, name: solver_name)

    %{
      solver: %{name: solver_name, pid: solver_pid},
      intent_pool: %{name: intent_pool_name, pid: intent_pool_pid}
    }
  end

  def cleanup(context) do
    Process.sleep(1000)
    GenServer.stop(context.solver.pid, :normal)
    GenServer.stop(context.intent_pool.pid, :normal)
  end

  ############################################################
  #                           Scenarios                      #
  ############################################################

  @doc """
  I create a single transaction and ask the solver to solve it.
  The transaction cannot be solved, so it should be in the unbalanced list
  when the solver returns.
  """
  @spec solve_transaction() :: :ok
  def solve_transaction() do
    # create an empty intent
    intent = %DumbIntent{}

    # solve the transaction
    assert Solver.solve([intent]) == [intent]
  end

  @doc """
  I solve multiple intents that are valid when composed.
  """
  @spec solve_transactions() :: :ok
  def solve_transactions() do
    # create an empty intent
    intent_1 = %DumbIntent{value: -1}
    intent_2 = %DumbIntent{value: 1}

    # solve the transaction
    expected = Enum.sort([intent_1, intent_2])
    result = Enum.sort(Solver.solve([intent_1, intent_2]))
    assert expected == result
  end

  @doc """
  I solve multiple intents that are valid when composed.
  """
  @spec solve_transactions_with_remainder() :: :ok
  def solve_transactions_with_remainder() do
    # create an empty intent
    intent_1 = %DumbIntent{value: -1}
    intent_2 = %DumbIntent{value: 1}
    intent_3 = %DumbIntent{value: 100}

    # solve the intents. only the first two can be solved.
    assert Enum.sort(Solver.solve([intent_1, intent_2, intent_3])) ==
             Enum.sort([intent_1, intent_2])
  end

  @doc """
  I insert a single transaction into the solver, which it cannot solve.
  I verify that the transaction is then in the unsolved list.
  """
  @spec solvable_transaction_via_intent_pool() :: :ok
  def solvable_transaction_via_intent_pool() do
    # startup
    context = initialization()
    solver = context.solver.pid
    intent_pool = context.intent_pool.pid

    # the solver does not have solved transactions.
    assert [] == Solver.get_solved(solver)
    assert [] == Solver.get_unsolved(solver)

    # add an intent to the pool
    # note: this is asynchronous, so block this process for a bit
    intent_1 = %DumbIntent{value: -1}
    IntentPool.new_intent(intent_pool, intent_1)
    Process.sleep(100)

    # the solver does not have solved transactions.
    assert Solver.get_solved(solver) == []
    assert Solver.get_unsolved(solver) == [intent_1]

    # --------------------------------------------------------------------------
    # add a second intent to make it solvable

    intent_2 = %DumbIntent{value: 1}
    IntentPool.new_intent(intent_pool, intent_2)
    Process.sleep(100)

    # the solver does not have solved transactions.
    assert Solver.get_solved(solver) == [intent_1, intent_2]
    assert Solver.get_unsolved(solver) == []

    # --------------------------------------------------------------------------
    # add a third intent to make it unsolvable

    intent_3 = %DumbIntent{value: 1000}
    IntentPool.new_intent(intent_pool, intent_3)
    Process.sleep(100)

    # the solver does not have solved transactions.
    assert Solver.get_solved(solver) == [intent_1, intent_2]
    assert Solver.get_unsolved(solver) == [intent_3]

    # cleanup
    cleanup(context)
  end
end
