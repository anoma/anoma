defmodule Anoma.Block do
  @moduledoc """

  I represent a block that will be stored and gossiped around the
  network.

  ### Fields

     - `:id` - Identification number derived from the serialization of my:
        + digest(block)
        + round
        + pub_key
     - `:block` - The block I contain
     - `:round` - The round I come from
     - `:pub_key` - Public key
     - `:signature` - Id signed with the private_key related to my public_key

  """

  use TypedStruct

  alias Anoma.{Block, Serializer}
  alias Anoma.Block.Base

  @type private_key() :: [:crypto.key_id()]
  @type public_key() :: [:crypto.key_id()]

  typedstruct do
    field(:id, binary(), require: true)
    field(:block, Base.t(), require: true)
    field(:round, non_neg_integer(), default: 0)
    field(:pub_key, Serializer.public_key(), require: true)
    field(:signature, binary())
  end

  @doc """
  I create a block, if a private key is passed in my second parameter,
  then I also contain a signature of myself.

  The signature (if created), represents that my `:pub_key` signed my
  `:id`. This can be cryptography verified with `Serializer.verify`

  ### Parameters

    - `block` - the base block that I am structured around
    - `key` - Either a public key or a public private key paring
    - `round` - The round that I represent
  """
  @spec create(
          Base.t(),
          Serializer.public_key() | {Serializer.public_key(), Serializer.private_key()},
          non_neg_integer()
        ) :: t()

  def create(block, {pub, priv}, round) do
    b = create(block, pub, round)
    %Block{b | signature: sign(b, priv)}
  end

  def create(block, pub, round) do
    %Block{block: block, pub_key: pub, id: signable(block, pub, round)}
  end

  @doc """
  I sign my id given a private key. I do no validation checking if the
  `id` is derived properly.
  """
  @spec sign(t(), Serializer.private_key()) :: binary()
  def sign(block, priv) do
    Serializer.sign(block.id, priv)
  end

  # create data which can easily be signed
  @spec signable(Base.t(), Serializer.public_key(), non_neg_integer()) :: binary()
  defp signable(block, pub_key, round) do
    Serializer.serialize({Base.digest(block), round, pub_key})
  end
end
