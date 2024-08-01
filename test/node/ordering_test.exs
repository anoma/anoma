defmodule AnomaTest.Node.Ordering do
  use TestHelper.TestMacro, async: true

  alias Examples.ENode.EOrdering

  test "examples" do
    EOrdering.miki_first()
    EOrdering.miki_also_second()
    EOrdering.miki_clear()
  end
end
