defmodule Examples.StartStateTest do
  use ExUnit.Case, async: true
  use TestHelper.TestMacro

  use TestHelper.GenerateExampleTests,
    for: Anoma.Node.Examples.EReplay.StartState
end
