defmodule Anoma.Node.Examples.EConsensus do
  alias Anoma.Node.Utility.Consensus
  alias Anoma.Node.Transaction.Mempool
  alias Anoma.Node.Examples.{ETransaction, ELogging}

  require EventBroker.Event

  require ExUnit.Assertions
  import ExUnit.Assertions

  def restart_consensus() do
    if GenServer.whereis(Consensus) do
      GenServer.stop(Consensus)
    end

    Consensus.start_link(100)
  end

  def restart_consensus_env() do
    ETransaction.restart_tx_module()
    ELogging.restart_logging()
    restart_consensus()
  end

  def startup_execution() do
    EventBroker.subscribe_me([])
    restart_consensus_env()

    assert_receive(
      %EventBroker.Event{
        body: %Mempool.BlockEvent{
          order: []
        }
      },
      5000
    )

    EventBroker.unsubscribe_me([])

    GenServer.stop(Consensus)
  end

  def execution_continues() do
    EventBroker.subscribe_me([])
    restart_consensus_env()

    Mempool.tx(ETransaction.zero(), "id 1")

    assert_receive(
      %EventBroker.Event{
        body: %Mempool.ConsensusEvent{
          order: []
        }
      },
      5000
    )

    assert_receive(
      %EventBroker.Event{
        body: %Mempool.BlockEvent{
          order: []
        }
      },
      5000
    )

    assert_receive(
      %EventBroker.Event{
        body: %Mempool.ConsensusEvent{
          order: ["id 1"]
        }
      },
      5000
    )

    assert_receive(
      %EventBroker.Event{
        body: %Mempool.BlockEvent{
          order: ["id 1"]
        }
      },
      5000
    )

    GenServer.stop(Consensus)
  end
end
