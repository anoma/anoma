defmodule AnomaTest.NockPoly.BinTreeTerm do
  use TestHelper.TestMacro, async: true

  use TestHelper.GenerateExampleTests,
    for: Examples.ENockPoly.EBinTreeTerm

  doctest(NockPoly.BinTreeTerm)
end
