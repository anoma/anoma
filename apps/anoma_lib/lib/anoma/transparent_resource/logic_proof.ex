defmodule Anoma.TransparentResource.LogicProof do
  use TypedStruct

  alias Anoma.TransparentResource.Resource
  alias __MODULE__

  typedstruct enforce: true do
    # the resource being proven
    field(:resource, Resource.t())
    # public inputs
    field(:commitments, MapSet.t(Resource.commitment()),
      default: MapSet.new()
    )

    field(:nullifiers, MapSet.t(Resource.nullifier()), default: MapSet.new())

    field(
      :self_tag,
      {:committed, Resource.commitment()}
      | {:nullified, Resource.commitment()}
    )

    field(:other_public, Noun.t(), default: <<>>)
    # private inputs
    field(:committed_plaintexts, MapSet.t(Resource.t()),
      default: MapSet.new()
    )

    field(:nullified_plaintexts, MapSet.t(Resource.t()),
      default: MapSet.new()
    )

    field(:other_private, Noun.t(), default: <<>>)
  end

  def verify(proof = %LogicProof{}) do
    args = []

    result = Nock.nock(proof.resource.logic, [9, 2, 10, [6, 1 | args], 0 | 1])

    case result do
      {:ok, zero} when zero in [0, <<>>, <<0>>, []] -> true
      _ -> false
    end
  end
end
