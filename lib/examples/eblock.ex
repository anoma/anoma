defmodule Examples.EBlock do
  alias Examples.ECrypto
  alias Anoma.Block

  require ExUnit.Assertions
  import ExUnit.Assertions

  @spec empty_block() :: Block.Base.t()
  def empty_block() do
    Block.Base.default()
  end

  @spec ablock() :: Block.t()
  def ablock() do
    block = Block.create(empty_block(), ECrypto.alice_rsa(), 0)

    assert block.signature != nil,
           "block creation with a private key leads to a signed block"

    assert block.id == Block.create(empty_block(), ECrypto.alice_rsa(), 0).id

    block
  end

  @spec bblock() :: Block.t()
  def bblock() do
    block = Block.create(empty_block(), ECrypto.bob_rsa(), 0)
    assert block != ablock()
    block
  end

  @spec apub_block() :: Block.t()
  def apub_block() do
    {pub, _priv} = ECrypto.alice_rsa()
    block = Block.create(empty_block(), pub, 0)

    assert nil == block.signature,
           "block creation with a public key leads to no signing"

    block
  end
end
