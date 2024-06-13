defmodule Anoma.Resource.ProofRecord do
  alias __MODULE__
  use TypedStruct

  alias Anoma.Resource
  alias Anoma.Resource.Proof

  typedstruct enforce: true do
    field(:proof, Proof.t())
    field(:cm_input, list(binary()), default: [])
    field(:nf_input, list(binary()), default: [])
  end

  @spec prove(Resource.t(), list(binary()), list(binary())) :: t()
  def prove(resource, cms, nfs) do
    %ProofRecord{
      proof: %Proof{
        resource: resource
      },
      cm_input: cms,
      nf_input: nfs
    }
  end

  @spec to_noun(t()) :: Noun.t()
  def to_noun(record) do
    [
      Resource.to_noun(record.proof.resource),
      record.cm_input | record.nf_input
    ]
  end

  @spec from_noun(Noun.t()) :: t()
  def from_noun([proof, cms | nfs]) do
    %ProofRecord{
      proof: %Proof{
        resource: Resource.from_noun(proof)
      },
      cm_input: cms,
      nf_input: nfs
    }
  end
end
