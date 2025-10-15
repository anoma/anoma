defmodule AnomaTest.NockPoly.BinTree do
  use TestHelper.TestMacro, async: true

  use TestHelper.GenerateExampleTests,
    for: Examples.ENockPoly.EBinTree

  import NockPoly.BinTree.MacroDefs
  doctest(NockPoly.BinTree)
  doctest(NockPoly.BinTree.MacroDefs)
end
