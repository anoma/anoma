defmodule AnomaTest.Serialisation do
  alias Examples.ESerialisation
  use TestHelper.TestMacro
  doctest Anoma.Serialise

  test "examples" do
    ESerialisation.empty_tx()
    ESerialisation.simple_map()
    ESerialisation.aresource()
  end
end
