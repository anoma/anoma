defmodule AnomaTest.NockPoly.FinIndIndPolyF do
  use TestHelper.TestMacro, async: true

  use TestHelper.GenerateExampleTests,
    for: Examples.ENockPoly.EFinIndIndPolyF

  doctest(NockPoly.FinIndIndPolyF)
end
