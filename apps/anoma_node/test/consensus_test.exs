defmodule ConsensusTest do
  use ExUnit.Case, async: false
  use TestHelper.GenerateExampleTests, for: Anoma.Node.Examples.EConsensus
end
