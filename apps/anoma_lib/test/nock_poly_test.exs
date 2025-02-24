defmodule AnomaTest.NockPoly do
  use TestHelper.TestMacro, async: true
  use TestHelper.GenerateExampleTests, for: Examples.ENockPoly

  doctest(NockPoly)
end
