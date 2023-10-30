defmodule AnomaTest.Block do
  use ExUnit.Case, async: true

  alias Anoma.Block
  alias Anoma.Block.Base

  doctest(Anoma.Block)

  test "deterministic hash" do
    keys = :crypto.generate_key(:rsa, {1024, 65537})
    block = Block.create(Base.default(), keys, 0)

    assert Block.create(Base.default(), keys, 0).id == block.id

    new_keys = :crypto.generate_key(:rsa, {1024, 65537})
    new_block = Block.create(Base.default(), new_keys, 0)
    assert block != new_block
  end

  test "no signature on startup" do
    {pub, priv} = :crypto.generate_key(:rsa, {1024, 65537})
    block = Block.create(Base.default(), pub, 0)
    assert block.signature == nil
  end
end
