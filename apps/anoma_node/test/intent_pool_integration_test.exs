defmodule IntentPoolIntegrationTest do
  use ExUnit.Case, async: false

  alias Anoma.Node.Examples.ENode
  alias Anoma.Node.Examples.ETransaction
  alias Anoma.Node.Intents.IntentPool
  alias Anoma.RM.DumbIntent
  alias Anoma.RM.Intent

  require Logger

  @moduledoc """
  Integration tests for IntentPool functionality.
  
  Tests concurrent operations, state consistency, and edge cases
  that aren't covered by the example-based tests.
  """

  setup do
    # ensure clean state for each test
    :ok
  end

  @tag :integration
  test "intent pool handles concurrent add operations" do
    enode = ENode.start_node()
    
    # spawn multiple processes adding intents simultaneously
    tasks = for i <- 1..5 do
      Task.async(fn ->
        intent = %DumbIntent{value: i}
        IntentPool.new_intent(enode.node_id, intent)
        intent
      end)
    end
    
    intents = Task.await_many(tasks, 1000)
    
    # verify all intents were added
    pool_intents = IntentPool.intents(enode.node_id)
    assert MapSet.size(pool_intents) == 5
    
    # verify each intent is present
    for intent <- intents do
      assert MapSet.member?(pool_intents, intent)
    end
    
    ENode.stop_node(enode)
  end

  @tag :integration
  test "intent removal during concurrent operations" do
    enode = ENode.start_node()
    
    # add initial intents
    intents = for i <- 1..3 do
      intent = %DumbIntent{value: i}
      IntentPool.new_intent(enode.node_id, intent)
      intent
    end
    
    # verify initial state
    assert MapSet.size(IntentPool.intents(enode.node_id)) == 3
    
    # remove one intent while adding another
    [remove_intent | _] = intents
    new_intent = %DumbIntent{value: 99}
    
    task1 = Task.async(fn ->
      IntentPool.remove_intent(enode.node_id, remove_intent)
    end)
    
    task2 = Task.async(fn ->
      IntentPool.new_intent(enode.node_id, new_intent)
    end)
    
    Task.await_many([task1, task2], 1000)
    
    final_intents = IntentPool.intents(enode.node_id)
    
    # should have 3 intents total
    assert MapSet.size(final_intents) == 3
    refute MapSet.member?(final_intents, remove_intent)
    assert MapSet.member?(final_intents, new_intent)
    
    ENode.stop_node(enode)
  end

  @tag :integration  
  test "intent pool state consistency with nullifier events" do
    enode = ENode.start_node()
    
    # add intent that will be nullified
    intent = ETransaction.nullify_intent_eph()
    IntentPool.new_intent(enode.node_id, intent)
    
    assert MapSet.size(IntentPool.intents(enode.node_id)) == 1
    
    # trigger nullifier event
    ETransaction.submit_successful_trivial_swap(enode.node_id)
    
    # wait for event processing
    Process.sleep(100)
    
    # intent should be removed
    assert Enum.empty?(IntentPool.intents(enode.node_id))
    
    ENode.stop_node(enode)
  end
end
