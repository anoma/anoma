defmodule AnomaTest.NockPoly.Fin2ForestPolyF do
  use TestHelper.TestMacro, async: true

  use TestHelper.GenerateExampleTests,
    for: Examples.ENockPoly.EFin2ForestPolyF

  doctest(NockPoly.Fin2ForestPolyF)
end
