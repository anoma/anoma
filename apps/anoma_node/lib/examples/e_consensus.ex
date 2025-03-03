defmodule Anoma.Node.Examples.EConsensus do
  alias Anoma.Node
  alias Anoma.Node.Events
  alias Node.Examples
  alias Examples.{ETransaction, ENode}
  alias Node.Transaction.Mempool
  alias Node.Utility.Consensus

  require EventBroker.Event

  require ExUnit.Assertions
  import ExUnit.Assertions

  def startup_execution(node_id \\ Node.example_random_id()) do
    ENode.start_node(node_id: node_id)

    {:ok, _consensus_pid} =
      Consensus.start_link(node_id: node_id, interval: 500)

    EventBroker.subscribe_me([])

    assert_receive(
      %EventBroker.Event{
        body: %Node.Event{
          node_id: ^node_id,
          body: %Events.BlockEvent{
            order: []
          }
        }
      },
      5000
    )

    EventBroker.unsubscribe_me([])
  end

  def execution_continues(node_id \\ Node.example_random_id()) do
    EventBroker.subscribe_me([])

    # start up a new node
    ENode.start_node(node_id: node_id)

    {:ok, _} = Consensus.start_link(node_id: node_id, interval: 500)

    assert_receive(
      %EventBroker.Event{
        body: %Node.Event{
          node_id: ^node_id,
          body: %Events.ConsensusEvent{
            order: []
          }
        }
      },
      5000
    )

    assert_receive(
      %EventBroker.Event{
        body: %Node.Event{
          node_id: ^node_id,
          body: %Events.BlockEvent{
            order: []
          }
        }
      },
      5000
    )

    Mempool.tx(node_id, ETransaction.zero(), "id 1")

    assert_receive(
      %EventBroker.Event{
        body: %Node.Event{
          node_id: ^node_id,
          body: %Events.ConsensusEvent{
            order: ["id 1"]
          }
        }
      },
      5000
    )

    assert_receive(
      %EventBroker.Event{
        body: %Node.Event{
          node_id: ^node_id,
          body: %Events.BlockEvent{
            order: ["id 1"]
          }
        }
      },
      5000
    )
  end
end
