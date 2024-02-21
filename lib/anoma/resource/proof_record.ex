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
        resource: resource
      }
    }
  end

  @spec to_noun(t()) :: Noun.t()
  def to_noun(record) do
    Resource.to_noun(record.proof.resource)
  end

  @spec from_noun(Noun.t()) :: t()
  def from_noun(record) do
    %ProofRecord{
      proof: %Proof{
        resource: Resource.from_noun(record)
      }
    }
  end
end
