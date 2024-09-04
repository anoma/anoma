defmodule Anoma.RM.Transparent.ProofRecord do
  alias __MODULE__
  use TypedStruct

  @behaviour Noun.Nounable.Kind

  alias Anoma.RM.Resource
  alias Anoma.RM.Transparent.Proof

  typedstruct enforce: true do
    field(:proof, Proof.t())
  end

  @spec prove(Resource.t()) :: t()
  def prove(resource) do
    %ProofRecord{
      proof: %Proof{
        resource: resource
      }
    }
  end

  @spec from_noun(Noun.t()) :: {:ok, t()} | :error
  def from_noun(record) do
    with {:ok, resource} <- Resource.from_noun(record) do
      {:ok,
       %ProofRecord{
         proof: %Proof{
           resource: resource
         }
       }}
    end
  end

  defimpl Noun.Nounable, for: __MODULE__ do
    def to_noun(record) do
      Noun.Nounable.to_noun(record.proof.resource)
    end
  end
end
