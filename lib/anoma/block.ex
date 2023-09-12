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
end
