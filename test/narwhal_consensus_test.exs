defmodule NarwhalConsensusTest do
  use ExUnit.Case, async: true

  use TestHelper.GenerateExampleTests,
    for: Anoma.Node.Examples.ENarwhal.Consensus
end
