defmodule AnomaTest.NockPoly.FinSlicePolyF do
  use TestHelper.TestMacro, async: true

  use TestHelper.GenerateExampleTests,
    for: Examples.ENockPoly.EFinSlicePolyF

  doctest(NockPoly.FinSlicePolyF)
end
