defmodule Anoma.ShieldedResource.ProofRecord do
  @moduledoc """
  I am a proof record for a shielded resource.
  """

  use TypedStruct

  typedstruct enforce: true do
    field(:proof, binary(), default: <<>>)
    field(:public_inputs, binary(), default: <<>>)
  end
end
