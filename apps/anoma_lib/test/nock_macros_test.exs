defmodule AnomaTest.NockMacros do
  use TestHelper.TestMacro, async: true
  use TestHelper.GenerateExampleTests, for: Examples.ENockMacros

  doctest(NockMacros)
end
