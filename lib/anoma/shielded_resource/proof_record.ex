defmodule Anoma.ShieldedResource.ProofRecord do
  @moduledoc """
  I am a proof record for a shielded resource.
  """

  alias __MODULE__
  use TypedStruct

  typedstruct enforce: true do
    field(:proof, binary(), default: <<>>)
    field(:public_inputs, binary(), default: <<>>)
  end

  @spec to_noun(t()) :: Noun.t()
  def to_noun(proof_record = %ProofRecord{}) do
    [
      proof_record.proof,
      proof_record.public_inputs
    ]
  end

  def from_noun([proof, public_inputs]) do
    %ProofRecord{
      proof: proof,
      public_inputs: public_inputs
    }
  end
end
