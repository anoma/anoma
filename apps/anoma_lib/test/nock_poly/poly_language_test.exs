defmodule AnomaTest.NockPoly.PolyLanguage do
  use TestHelper.TestMacro, async: true

  use TestHelper.GenerateExampleTests,
    for: Examples.ENockPoly.EPolyLanguage

  doctest(NockPoly.PolyLanguage)
end
