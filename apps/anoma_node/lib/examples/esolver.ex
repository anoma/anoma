defmodule Anoma.Node.Examples.ESolver do
  @moduledoc """
  I contain several examples on how to use the solver.
  """

  require ExUnit.Assertions
  import ExUnit.Assertions

  alias Anoma.Node.Intents.IntentPool
  alias Anoma.Node.Intents.Solver
  alias Anoma.RM.DumbIntent
  alias Anoma.Node.Examples.ENode

  ############################################################
  #                           Scenarios                      #
  ############################################################

  @doc """
  I create a single transaction and ask the solver to solve it.
  The transaction cannot be solved, so it should be in the unbalanced list
  when the solver returns.
  """
  @spec solve_transaction() :: boolean()
  def solve_transaction() do
    # create an empty intent
    intent = %DumbIntent{}

    # solve the transaction
    assert Solver.solve([intent]) == MapSet.new([intent])
  end

  @doc """
  I solve multiple intents that are valid when composed.
  """
  @spec solve_transactions() :: boolean()
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
  @spec solve_transactions_with_remainder() :: boolean()
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
  @spec solvable_transaction_via_intent_pool(ENode.t()) :: boolean()
  def solvable_transaction_via_intent_pool(enode \\ ENode.start_node()) do
    # startup
    # the solver does not have solved transactions.
    assert [] == Solver.get_solved(enode.node_id)
    assert [] == Solver.get_unsolved(enode.node_id)

    # add an intent to the pool
    # note: this is asynchronous, so block this process for a bit
    intent_1 = %DumbIntent{value: -1}
    IntentPool.new_intent(enode.node_id, intent_1)
    Process.sleep(100)

    # the solver does not have solved transactions.
    assert Solver.get_solved(enode.node_id) == []
    assert Solver.get_unsolved(enode.node_id) == [intent_1]

    # --------------------------------------------------------------------------
    # add a second intent to make it solvable

    intent_2 = %DumbIntent{value: 1}
    IntentPool.new_intent(enode.node_id, intent_2)
    Process.sleep(100)

    # the solver does not have solved transactions.
    assert Solver.get_solved(enode.node_id) == [intent_1, intent_2]
    assert Solver.get_unsolved(enode.node_id) == []

    # --------------------------------------------------------------------------
    # add a third intent to make it unsolvable

    intent_3 = %DumbIntent{value: 1000}
    IntentPool.new_intent(enode.node_id, intent_3)
    Process.sleep(100)

    # the solver does not have solved transactions.
    assert Solver.get_solved(enode.node_id) == [intent_1, intent_2]
    assert Solver.get_unsolved(enode.node_id) == [intent_3]
  end
end
