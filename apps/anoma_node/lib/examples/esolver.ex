defmodule Anoma.Node.Examples.ESolver do
  @moduledoc """
  I contain several examples on how to use the solver.
  """

  alias Anoma.Node.Examples.ENode
  alias Anoma.Node.Intents.IntentPool
  alias Anoma.Node.Intents.Solver
  alias Anoma.Node.Transaction.Mempool
  alias Anoma.RM.DumbIntent
  alias Anoma.Node.Events

  require ExUnit.Assertions

  import ExUnit.Assertions

  use EventBroker.WithSubscription

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
    assert [] == Solver.get_unsolved(enode.node_id)

    # add an intent to the pool
    # note: this is asynchronous, so block this process for a bit
    intent_1 = %DumbIntent{value: -1}
    IntentPool.new_intent(enode.node_id, intent_1)
    Process.sleep(100)

    # the solver does not have solved transactions.
    assert Solver.get_unsolved(enode.node_id) == [intent_1]

    # --------------------------------------------------------------------------
    # add a second intent to make it solvable

    intent_2 = %DumbIntent{value: 1}
    IntentPool.new_intent(enode.node_id, intent_2)
    Process.sleep(100)

    # the solver does not have unsolved transactions.
    assert Solver.get_unsolved(enode.node_id) == []

    # --------------------------------------------------------------------------
    # add a third intent to make it unsolvable

    intent_3 = %DumbIntent{value: 1000}
    IntentPool.new_intent(enode.node_id, intent_3)
    Process.sleep(100)

    # the solver does not have solved transactions.
    assert Solver.get_unsolved(enode.node_id) == [intent_3]
  end

  @doc """
  I send a trivial transaction to the Intent Pool, which then gets solved
  by itself and then sent to the Mempool to be executed.
  """
  @spec solvable_transaction_gets_executed(ENode.t()) :: :ok
  def solvable_transaction_gets_executed(enode \\ ENode.start_node()) do
    node_id = enode.node_id
    assert [] == Solver.get_unsolved(node_id)

    tx = Examples.ETransparent.ETransaction.swap_from_actions()

    IntentPool.new_intent(node_id, tx)

    tx_candidate = [
      [1, 0, [1 | tx |> Noun.Nounable.to_noun()], 0 | 909],
      0 | 707
    ]

    tx_filter = [Anoma.Node.Event.node_filter(node_id), %Mempool.TxFilter{}]

    with_subscription [tx_filter] do
      Mempool.tx(
        node_id,
        {:transparent_resource, tx_candidate}
      )

      :ok =
        receive do
          %EventBroker.Event{
            body: %Anoma.Node.Event{
              node_id: ^node_id,
              body: %Events.TxEvent{
                tx: %Mempool.Tx{backend: _, code: ^tx_candidate}
              }
            }
          } ->
            :ok
        after
          1000 -> :error
        end
    end
  end
end
