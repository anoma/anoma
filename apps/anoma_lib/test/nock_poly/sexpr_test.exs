defmodule AnomaTest.NockPoly.Sexpr do
  use TestHelper.TestMacro, async: true

  use TestHelper.GenerateExampleTests,
    for: Examples.ENockPoly.ESexpr

  doctest(NockPoly.Sexpr)
  doctest(NockPoly.Sexpr.MacroDefs)
end
