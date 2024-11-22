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
    assert Enum.empty?(IntentPool.intents(enode.node_id))

    enode
  end

  @doc """
  I submit a transaction containing a nullifier to an ephemeral resource
  used in a trivial swap transaction.
  """
  @spec add_intent_transaction_nullifier(ENode.t()) :: ENode.t()
  def add_intent_transaction_nullifier(enode \\ ENode.start_node()) do
    intent = Examples.ETransparent.ETransaction.nullify_intent_eph()
    node_id = enode.node_id
    IntentPool.new_intent(node_id, intent)

    assert IntentPool.intents(node_id) == MapSet.new([intent])

    enode
  end

  @doc """
  I use `add_intent_transaction_nullifier` and then execute the trivial
  swap.

  The nullifier of the ephemeral resource used gets trasmitted to the
  intent pool, hence removing the specified intent from the pool.
  """
  @spec remove_intents_with_nulllified_resources(ENode.t()) :: ENode.t()
  def remove_intents_with_nulllified_resources(enode \\ ENode.start_node()) do
    add_intent_transaction_nullifier(enode)

    Anoma.Node.Examples.ETransaction.submit_successful_trivial_swap(
      enode.node_id
    )

    assert enode.node_id |> IntentPool.intents() |> Enum.empty?()

    enode
  end
end
