defmodule AnomaTest.Block do
  use TestHelper.TestMacro, async: true

  alias Examples.EBlock

  doctest(Anoma.Block)

  test "examples" do
    EBlock.ablock()
    EBlock.bblock()
    EBlock.apub_block()
  end
end
