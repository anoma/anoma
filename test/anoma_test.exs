defmodule AnomaTest do
  use ExUnit.Case
  doctest Anoma

  test "greets the world" do
    assert Anoma.hello() == :world
  end
end
