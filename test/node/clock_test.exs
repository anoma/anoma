defmodule AnomaTest.Node.Clock do
  use TestHelper.TestMacro, async: true

  alias Examples.ENode.EClock

  test "examples" do
    EClock.start_clock()
  end
end
