defmodule Anoma.Node.Examples.EReplay do
  @moduledoc """
  I define examples that test the behavior of the replay mechanism.
  """

  alias Anoma.Node.Examples.ENode
  alias Anoma.Node.Replay
  alias Anoma.Node.Transaction.Backends
  alias Anoma.Node.Examples.EReplay

  use EventBroker.WithSubscription

  require Logger

  import ExUnit.Assertions
  import Mock

  @doc """
  Given a node, I calculate its startup arguments and try to start a new node with them.
  """
  @spec replay_succeeds(ENode.t()) :: ENode.t()
  def replay_succeeds(enode \\ ENode.start_node()) do
    # try and replay this node.
    assert Kernel.match?({:ok, _}, Replay.replay_for(enode.node_id))

    # start the previous node again.
    ENode.start_node(node_id: enode.node_id, grpc_port: enode.grpc_port)
  end

  @doc """
  I execute replay on a node that has a transaction in its mempool.
  """
  @spec replay_with_transaction(ENode.t()) :: ENode.t()
  def replay_with_transaction(enode \\ ENode.start_node()) do
    # run a transaction in the node, but avoid it from creating a block.
    # this yields a mempool that should start the replay with a given consensus.
    {_node, _transaction} = EReplay.StartState.mempool_todo_consensus(enode)

    result = Replay.replay_for(enode.node_id)

    assert result == {:ok, :replay_succeeded}

    enode
  end

  @doc """
  I execute replay on a node that has a transaction in its mempool.
  """
  @spec replay_with_faulty_transaction(ENode.t()) :: ENode.t()
  def replay_with_faulty_transaction(enode \\ ENode.start_node()) do
    # run a transaction in the node, but avoid it from creating a block.
    # this yields a mempool that should start the replay with a given consensus.
    {_node, _transaction} = EReplay.StartState.mempool_todo_consensus(enode)

    # run the replay, but make sure that the transaction crashes
    execute_fn = fn _, _, _ -> raise "An error during computation" end

    with_mock Backends, execute: execute_fn do
      with_subscription [[]] do
        assert {:error, :replay_failed} == Replay.replay_for(enode.node_id)
      end
    end

    enode
  end
end
