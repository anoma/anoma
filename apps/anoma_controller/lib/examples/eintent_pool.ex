defmodule Anoma.Controller.Examples.EIntentPool do
  @moduledoc """
  I contain several examples on how to run the intent pool.
  """

  alias Anoma.Controller
  alias Anoma.Controller.Examples.EController
  alias Anoma.Controller.Intents.IntentPool
  alias Anoma.Controller.Tables
  alias Anoma.RM.DumbIntent
  alias Anoma.RM.Intent

  require ExUnit.Assertions
  require Controller.Event

  import ExUnit.Assertions

  ############################################################
  #                           Scenarios                      #
  ############################################################

  @doc """
  I check that the intent pool returns an empty map of intents when its started.
  """
  @spec list_intents(EController.t()) :: EController.t()
  def list_intents(enode \\ EController.start_node()) do
    assert MapSet.new() == IntentPool.intents(enode.node_id)
    enode
  end

  @doc """
  I check that when an intent is added to the pool, is is present in the mapset.
  """
  @spec add_intent(EController.t()) :: EController.t()
  def add_intent(enode \\ EController.start_node()) do
    intent = %DumbIntent{}
    IntentPool.new_intent(enode.node_id, intent)

    # the intent will be present in the mapset.
    assert Enum.count(IntentPool.intents(enode.node_id)) == 1

    enode
  end

  @doc """
  I add and remove an intent from the pool and ensure that it's actually gone.
  """
  @spec remove_intent(EController.t()) :: EController.t()
  def remove_intent(enode \\ EController.start_node()) do
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
  @spec add_intent_transaction_nullifier(EController.t()) :: EController.t()
  def add_intent_transaction_nullifier(enode \\ EController.start_node()) do
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
  @spec remove_intents_with_nulllified_resources(EController.t()) ::
          EController.t()
  def remove_intents_with_nulllified_resources(
        enode \\ EController.start_node()
      ) do
    add_intent_transaction_nullifier(enode)

    Anoma.Controller.Examples.ETransaction.submit_successful_trivial_swap(
      enode.node_id
    )

    assert enode.node_id |> IntentPool.intents() |> Enum.empty?()

    enode
  end

  @doc """
  I check that adding an intent with nullifiers already known in the nlfs_set
  does not add the intent to the pool.
  """
  @spec add_intent_with_known_nullifiers(EController.t()) :: EController.t()
  def add_intent_with_known_nullifiers(enode \\ EController.start_node()) do
    intent = Examples.ETransparent.ETransaction.nullify_intent_eph()
    nlfs_set = Intent.nullifiers(intent)

    new_nullifiers_event(enode, nlfs_set)

    Process.sleep(100)

    node_id = enode.node_id
    IntentPool.new_intent(node_id, intent)

    # the intent will not be present in the mapset
    assert IntentPool.intents(node_id) == MapSet.new([])

    enode
  end

  @doc """
  I check that adding an intent with commitments already known in the state
  does not add the intent to the pool.
  """
  @spec add_intent_with_submitted_commitments(EController.t()) ::
          EController.t()
  def add_intent_with_submitted_commitments(enode \\ EController.start_node()) do
    intent = Examples.ETransparent.ETransaction.single_swap()
    cms_set = Intent.commitments(intent)

    new_commitments_event(enode, cms_set)

    Process.sleep(100)

    node_id = enode.node_id
    IntentPool.new_intent(node_id, intent)

    # the intent will not be present in the mapset
    assert IntentPool.intents(node_id) == MapSet.new([])

    enode
  end

  def intents_are_written(enode \\ EController.start_node()) do
    add_intent_transaction_nullifier(enode)

    table = Tables.table_intents(enode.node_id)

    pool = IntentPool.intents(enode.node_id)

    assert {:atomic, [{^table, "intents", ^pool}]} =
             :mnesia.transaction(fn -> :mnesia.read(table, "intents") end)
  end

  @doc """
  I submit a nullifier event to the node.
  """
  @spec new_nullifiers_event(EController.t(), MapSet.t()) :: EController.t()
  def new_nullifiers_event(
        enode \\ EController.start_node(),
        nlfs_set \\ MapSet.new()
      ) do
    node_id = enode.node_id

    event =
      Controller.Event.new_with_body(
        node_id,
        %Anoma.Controller.Transaction.Backends.TRMEvent{
          nullifiers: nlfs_set,
          commitments: MapSet.new([])
        }
      )

    EventBroker.event(event)

    enode
  end

  @doc """
  I submit a commitment event to the node.
  """
  @spec new_commitments_event(EController.t(), MapSet.t()) :: EController.t()
  def new_commitments_event(
        enode \\ EController.start_node(),
        cms_set \\ MapSet.new()
      ) do
    node_id = enode.node_id

    event =
      Controller.Event.new_with_body(
        node_id,
        %Anoma.Controller.Transaction.Backends.TRMEvent{
          nullifiers: MapSet.new([]),
          commitments: cms_set
        }
      )

    EventBroker.event(event)

    enode
  end
end
