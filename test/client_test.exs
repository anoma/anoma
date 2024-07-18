defmodule AnomaTest.Client do
  use TestHelper.TestMacro, async: true

  alias Examples.EClient

  doctest(Anoma.Block)

  test "examples" do
    EClient.get_from_other()
    EClient.storage_423_from_cli()
  end
end
