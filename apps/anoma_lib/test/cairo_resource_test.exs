defmodule AnomaTest.CairoResource do
  use TestHelper.TestMacro, async: true

  @moduletag :zk

  use TestHelper.GenerateExampleTests,
    for: Examples.ECairo.EComplianceWitness

  use TestHelper.GenerateExampleTests,
    for: Examples.ECairo.EAction

  use TestHelper.GenerateExampleTests,
    for: Examples.ECairo.ETransaction

  use TestHelper.GenerateExampleTests, for: Examples.ECairo.EResource

  use TestHelper.GenerateExampleTests,
    for: Examples.ECairo.EResourceLogic
end
