defmodule AnomaTest.TransparentResourceMachine do
  alias Anoma.Examples

  use TestHelper.TestMacro, async: true
  use TestHelper.GenerateExampleTests, for: Examples.ETransparent.EResource
  use TestHelper.GenerateExampleTests, for: Examples.ETransparent.ELogicProof
  use TestHelper.GenerateExampleTests, for: Examples.ETransparent.EAction
  use TestHelper.GenerateExampleTests, for: Examples.ETransparent.ETransaction
end
