defmodule LoggingTest do
  use ExUnit.Case, async: false

  alias Anoma.Node.Examples.ELogging

  test "transaction event examples" do
    ELogging.check_tx_event()
    ELogging.check_multiple_tx_events()
  end

  test "consensus event examples" do
    ELogging.check_consensus_event()
    ELogging.check_consensus_event_multiple()
  end

  test "block event examples" do
    ELogging.check_block_event()
    ELogging.check_block_event_multiple()
    ELogging.check_block_event_leave_one_out()
  end

  test "replay examples" do
    ELogging.replay_tx()
    ELogging.replay_several_txs()
    ELogging.replay_consensus()
    ELogging.replay_consensus_with_several_txs()
    ELogging.replay_several_consensus()
    ELogging.replay_consensus_leave_one_out()
    ELogging.replay_corrects_result()
  end
end
