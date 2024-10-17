defmodule AnomaTest.Nock do
  use TestHelper.TestMacro, async: true
  use TestHelper.GenerateExampleTests, for: Examples.ENock

  doctest(Nock)
  doctest(Noun)
  doctest(Noun.Format)
end
