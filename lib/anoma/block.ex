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

  ######################################################################
  ## Mnesia
  ######################################################################

  # this should stay in-sync with the type declaration

  defp attributes, do: [:id, :block, :round, :pub_key, :signature]

  @spec encode(t()) :: tuple()
  def encode(block) do
    {__MODULE__, block.id, block.block, block.round, block.pub_key, block.signature}
  end

  def decode({__MODULE__, id, block, round, pub_key, sig}) do
    %Block{id: id, block: block, round: round, pub_key: pub_key, signature: sig}
  end

  @doc """

  I create a `:mnesia` table for `Anoma.Block`. This table is backed
  by rocksdb, and thus persists across IEX sessions.

  I will only ever needed to be called once upon Configuration start,
  `Anoma.Mnesia.init/0` will likely set me up as is.

  """
  def create_table() do
    create_table(Anoma.Block, true)
  end

  @doc """

  I am like `create_table/0`, however I am given a special
  table_key. This overrides the default table key of `Anoma.Block`.

  I am useful when trying to spawn many solvers/validators/etc, who
  all want their own tables.

  ### Parameters

    - `table_key` - the name of the table
    - `rocks?` - should we persist as a rocksdb table?

  """
  def create_table(table_key, rocks?) do
    resp =
      if rocks? do
        :mnesia.create_table(table_key, attributes: attributes(), rocksdb_copies: [node()])
      else
        :mnesia.create_table(table_key, attributes: attributes())
      end

    :mnesia.add_table_index(table_key, :round)
    resp
  end
end
