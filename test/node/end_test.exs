defmodule AnomaTest.Node.End do
  use TestHelper.TestMacro, async: true

  alias Examples.ENode.EIntent

  test "examples" do
    EIntent.solved_trade()
  end
end
