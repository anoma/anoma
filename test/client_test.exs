defmodule AnomaTest.Client do
  use TestHelper.TestMacro, async: true

  alias Examples.EClient

  doctest(Anoma.Block)

  test "examples" do
    EClient.get_from_other()
  end
end
