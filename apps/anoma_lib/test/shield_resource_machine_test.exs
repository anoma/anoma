defmodule AnomaTest.ShieldResourceMachine do
  use TestHelper.TestMacro, async: true

  @moduletag :zk

  use TestHelper.GenerateExampleTests,
    for: Examples.ERM.EShielded.EComplianceInput

  use TestHelper.GenerateExampleTests,
    for: Examples.ERM.EShielded.EPartialTransaction

  use TestHelper.GenerateExampleTests,
    for: Examples.ERM.EShielded.ETransaction

  use TestHelper.GenerateExampleTests, for: Examples.ERM.EShielded.EResource

  use TestHelper.GenerateExampleTests,
    for: Examples.ERM.EShielded.EResourceLogic
end
