defmodule Anoma.Resource.ProofRecord do
  alias __MODULE__
  use TypedStruct

  alias Anoma.Resource
  alias Anoma.Resource.Proof

  typedstruct enforce: true do
    field(:proof, Proof.t(), default: nil)
  end

  @spec prove(Resource.t()) :: t()
  def prove(resource) do
    %ProofRecord{
      proof: %Proof{
        proof_value: {:transparent, resource}
      }
    }
  end

  @spec to_noun(t()) :: Noun.t()
  def to_noun(%ProofRecord{
        proof: %Proof{proof_value: {:transparent, resource}}
      }) do
    Resource.to_noun(resource)
  end

  @spec from_noun(Noun.t()) :: t()
  def from_noun(record) do
    %ProofRecord{
      proof: %Proof{
        proof_value: {:transparent, Resource.from_noun(record)}
      }
    }
  end
end
