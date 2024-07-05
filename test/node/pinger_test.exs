defmodule AnomaTest.Node.Pinger do
  use TestHelper.TestMacro, async: true

  alias Examples.ENode.EPinger

  test "examples" do
    EPinger.pinger_run()
  end
end
