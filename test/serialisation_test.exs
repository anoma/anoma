defmodule SerialisationTest do
  use ExUnit.Case

  test "basic" do
    r = %Anoma.Types.Resource{rseed: "a", npk: "b", nonce: "c"}
    assert r == Anoma.Types.Resource.deserialise(Anoma.Types.Resource.serialise(r))
  end
end
