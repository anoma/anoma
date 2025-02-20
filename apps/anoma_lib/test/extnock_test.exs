defmodule AnomaTest.ExtNock do
  use TestHelper.TestMacro, async: true
  use TestHelper.GenerateExampleTests, for: Examples.EExtNock

  doctest(ExtNock)
end
