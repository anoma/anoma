defmodule AnomaClientLocalTest do
  use ExUnit.Case
  doctest AnomaClientLocal

  test "greets the world" do
    assert AnomaClientLocal.hello() == :world
  end
end
