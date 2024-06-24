defmodule SerialisationTest do
  use TestHelper.TestMacro
  doctest Anoma.Serialise

  test "basic" do
    # test from_msgpack o to_msgpack = id
    tid = fn x ->
      x ===
        elem(
          Anoma.Serialise.from_msgpack(
            elem(
              :msgpack.unpack(:msgpack.pack(Anoma.Serialise.to_msgpack(x))),
              1
            )
          ),
          1
        )
    end

    assert tid.(%Anoma.Resource.Transaction{})
    assert tid.(Anoma.Resource.new())
    assert tid.(%{[1, 2 | 3] => 5})
  end
end
