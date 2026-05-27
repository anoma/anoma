defmodule Examples.AdvertiseTest do
  use ExUnit.Case, async: false
  use TestHelper.TestMacro

  # Skip the peer-spawning examples: they rename this BEAM and break
  # storage for later in-VM tests. The macro option is `:skip` (not
  # `:exclude`, which it silently ignores).
  use TestHelper.GenerateExampleTests,
    for: Anoma.Node.Examples.EAdvertise,
    skip: [:start_slave, :seed_nodes_distributed]
end
