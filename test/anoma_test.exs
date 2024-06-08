defmodule AnomaTest do
  use TestHelper.TestMacro, async: true

  doctest Anoma

  test "greets the world" do
    assert Anoma.hello() == :world
  end
end
