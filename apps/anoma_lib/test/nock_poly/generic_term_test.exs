defmodule AnomaTest.NockPoly.GenericTerm do
  use TestHelper.TestMacro, async: true

  use TestHelper.GenerateExampleTests,
    for: Examples.ENockPoly.EGenericTerm

  import NockPoly.GenericTerm
  doctest(NockPoly.GenericTerm)
end
