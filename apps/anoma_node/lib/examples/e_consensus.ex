defmodule Anoma.Node.Examples.EConsensus do
  alias Anoma.Node
  alias Node.{Examples, Registry}
  alias Examples.{ETransaction, ELogging, ENode}
  alias Node.Transaction.Mempool
  alias Node.Utility.Consensus

  require EventBroker.Event

  require ExUnit.Assertions
  import ExUnit.Assertions

  def restart_consensus(node_id \\ "londo_mollari") do
    stop_consensus(node_id)

    Consensus.start_link(node_id: node_id, interval: 100)
  end

  def stop_consensus(node_id \\ "londo_mollari") do
    pid = Registry.whereis(node_id, Consensus)

    if pid && Process.alive?(pid) do
      GenServer.stop(pid)
    end

    # pid = Registry.whereis(node_id, Mempool)

    # if pid && Process.alive?(pid) do
    #   GenServer.stop(pid)
    # end
  end

  def restart_consensus_env(node_id \\ "londo_mollari") do
    ETransaction.restart_tx_module(node_id)
    ELogging.restart_logging()
    restart_consensus(node_id)
  end

  def startup_execution() do
    node_id =
      ("londo_mollari" <> :crypto.strong_rand_bytes(16))
      |> Base.url_encode64()

    # start up a new node
    node_pid = ENode.start_node(node_id: node_id)

    {:ok, consensus_pid} =
      Consensus.start_link(node_id: node_id, interval: 1000)

    EventBroker.subscribe_me([])

    assert_receive(
      %EventBroker.Event{
        body: %Node.Event{
          node_id: ^node_id,
          body: %Mempool.BlockEvent{
            order: []
          }
        }
      },
      5000
    )

    EventBroker.unsubscribe_me([])

    GenServer.stop(consensus_pid)
    ENode.stop_node(node_pid)
  end

  # def execution_continues(node_id \\ "londo_mollari") do
  def execution_continues() do
    DynamicSupervisor.which_children(Anoma.Node.NodeSupervisor)
    |> Enum.map(fn child ->
      case child do
        {_, pid, _, _} ->
          Supervisor.stop(pid, :normal)
      end
    end)

    Process.sleep(5000)

    node_id =
      ("londo_mollari" <> :crypto.strong_rand_bytes(16))
      |> Base.url_encode64()

    EventBroker.subscribe_me([])

    # start up a new node
    ENode.start_node(node_id: node_id)

    {:ok, _} = Consensus.start_link(node_id: node_id, interval: 1000)

    Mempool.tx(node_id, ETransaction.zero(), "id 1")

    assert_receive(
      %EventBroker.Event{
        body: %Node.Event{
          node_id: ^node_id,
          body: %Mempool.ConsensusEvent{
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
          body: %Mempool.BlockEvent{
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
          body: %Mempool.ConsensusEvent{
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
          body: %Mempool.BlockEvent{
            order: ["id 1"]
          }
        }
      },
      5000
    )

    # ENode.stop_node(node_pid)
    # GenServer.stop(consensus_pid)
  end
end
