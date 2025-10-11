defmodule AnomaTest.NockPoly.Term do
  use TestHelper.TestMacro, async: true
  use TestHelper.GenerateExampleTests, for: Examples.ENockPoly.ETerm

  doctest(NockPoly.Term)
end
