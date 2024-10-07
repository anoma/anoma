defmodule ConsensusTest do
  use ExUnit.Case, async: false

  alias Anoma.Node.Examples.EConsensus

  test "consensus examples" do
    EConsensus.startup_execution()
    EConsensus.execution_continues()
  end
end
