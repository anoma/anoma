defmodule AnomaTest.NockPoly.NockTerms do
  use TestHelper.TestMacro, async: true

  use TestHelper.GenerateExampleTests,
    for: Examples.ENockPoly.ENockTerms

  doctest(NockPoly.NockTerms)
end
