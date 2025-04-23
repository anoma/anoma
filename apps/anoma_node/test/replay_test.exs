defmodule ReplayTest do
  use ExUnit.Case, async: true

  use TestHelper.GenerateExampleTests,
    for: Anoma.Node.Examples.EReplay.StartState

  use TestHelper.GenerateExampleTests,
    for: Anoma.Node.Examples.EReplay
end
