defmodule AnomaTest.ExtNock.ExtNockTerms do
  use TestHelper.TestMacro, async: true
  use TestHelper.GenerateExampleTests, for: Examples.EExtNock.EExtNockTerms

  doctest(ExtNock.ExtNockTerms)
end
