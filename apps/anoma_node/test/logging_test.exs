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
end
