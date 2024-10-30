defmodule AnomaTest.CairoResource do
  use TestHelper.TestMacro, async: true

  @moduletag :zk

  use TestHelper.GenerateExampleTests,
    for: Examples.ECairo.EComplianceInput

  use TestHelper.GenerateExampleTests,
    for: Examples.ECairo.EPartialTransaction

  use TestHelper.GenerateExampleTests,
    for: Examples.ECairo.ETransaction

  use TestHelper.GenerateExampleTests, for: Examples.ECairo.EResource
end
