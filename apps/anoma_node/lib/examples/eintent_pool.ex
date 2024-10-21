defmodule Anoma.Node.Examples.EIntentPool do
  @moduledoc """
  I contain several examples on how to run the intent pool.
  """

  require ExUnit.Assertions
  import ExUnit.Assertions

  alias Anoma.Node.Intents.IntentPool
  alias Anoma.RM.DumbIntent
  alias Anoma.Node.Examples.ENode

  ############################################################
  #                           Scenarios                      #
  ############################################################

  @doc """
  I check that the intent pool returns an empty map of intents when its started.
  """
  @spec list_intents(ENode.t()) :: ENode.t()
  def list_intents(enode \\ ENode.start_node()) do
    assert MapSet.new() == IntentPool.intents(enode.node_id)
    enode
  end

  @doc """
  I check that when an intent is added to the pool, is is present in the mapset.
  """
  @spec add_intent(ENode.t()) :: ENode.t()
  def add_intent(enode \\ ENode.start_node()) do
    intent = %DumbIntent{}
    IntentPool.new_intent(enode.node_id, intent)

    # the intent will be present in the mapset.
    assert Enum.count(IntentPool.intents(enode.node_id)) == 1

    enode
  end

  @doc """
  I add and remove an intent from the pool and ensure that it's actually gone.
  """
  @spec remove_intent(ENode.t()) :: ENode.t()
  def remove_intent(enode \\ ENode.start_node()) do
    # add an intent to the pool
    intent = %DumbIntent{}
    IntentPool.new_intent(enode.node_id, intent)

    # the intent will be present in the mapset.
    assert Enum.count(IntentPool.intents(enode.node_id)) == 1

    # remove the intent from the pool
    IntentPool.remove_intent(enode.node_id, intent)

    # the intent will be removed from the mapset.
    assert Enum.count(IntentPool.intents(enode.node_id)) == 0

    enode
  end
end
