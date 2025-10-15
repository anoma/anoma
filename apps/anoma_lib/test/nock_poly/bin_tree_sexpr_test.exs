defmodule AnomaTest.NockPoly.BinTreeSexpr do
  use TestHelper.TestMacro, async: true

  use TestHelper.GenerateExampleTests,
    for: Examples.ENockPoly.EBinTreeSexpr

  doctest(NockPoly.BinTreeSexpr)
end
