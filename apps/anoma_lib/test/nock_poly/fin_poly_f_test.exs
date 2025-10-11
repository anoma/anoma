defmodule AnomaTest.NockPoly.FinPolyF do
  use TestHelper.TestMacro, async: true
  use TestHelper.GenerateExampleTests, for: Examples.ENockPoly.EFinPolyF

  doctest(NockPoly.FinPolyF)
end
