defmodule Anoma.RM.Transparent.Resource do
  alias __MODULE__
  alias Anoma.RM.Transparent.Primitive.DeltaHash

  use TypedStruct

  typedstruct enfore: true do
    # the sha of the jam of a resource logic
    # avaliable from app data
    field(:logicref, <<_::256>>,
      default: :crypto.hash(:sha256, [[1 | 0], 0 | 0] |> Noun.Jam.jam())
    )

    # jammed
    field(:labelref, integer(), default: 2)
    # jammed
    field(:valueref, integer(), default: 2)
    field(:quantity, integer(), default: 1)
    # whether the resource is ephemetal or not
    field(:isephemeral, bool(), default: true)
    field(:nonce, <<_::256>>, default: <<0::256>>)
    # a commitment to the nullifier key
    # with the commitment hash being just the identity function
    field(:nullifierkeycommitment, Anoma.Crypto.Sign.public(),
      default: <<0::256>>
    )

    field(:randseed, <<>>, default: <<>>)
  end

  @spec commitment_hash(t()) :: integer()
  def commitment_hash(resource) do
    binary_resource = resource |> to_noun() |> Noun.Jam.jam()
    ("CM_" <> binary_resource) |> Noun.atom_binary_to_integer()
  end

  @spec nullifier_hash(<<_::256>>, integer()) :: integer()
  def nullifier_hash(_nullifier_key, resource) do
    binary_resource = resource |> to_noun() |> Noun.Jam.jam()
    ("NF_" <> binary_resource) |> Noun.atom_binary_to_integer()
  end

  @spec delta(t()) :: integer()
  def delta(resource) do
    %{kind(resource) => resource.quantity}
    |> DeltaHash.hash()
  end

  @spec kind(t()) :: <<_::256>>
  def kind(%Resource{labelref: label, logicref: logic}) do
    kind = [label | logic] |> Noun.Jam.jam()
    :crypto.hash(:sha256, kind)
  end

  @spec to_noun(Resource.t()) :: Noun.t()
  def to_noun(resource = %Resource{}) do
    [
      resource.logicref,
      resource.labelref,
      resource.valueref,
      resource.quantity,
      Noun.Nounable.to_noun(resource.isephemeral),
      resource.nonce,
      resource.nullifierkeycommitment
      | resource.randseed
    ]
  end

  @spec from_noun(Noun.t()) :: :error | {:ok, t()}
  def from_noun([
        logicref,
        labelref,
        valueref,
        quantity,
        ephemerality,
        nonce,
        nullifierkeycm
        | rseed
      ]) do
    # we make sure the types are respected
    {:ok,
     %Resource{
       logicref: Noun.atom_integer_to_binary(logicref),
       labelref: Noun.atom_binary_to_integer(labelref),
       valueref: Noun.atom_binary_to_integer(valueref),
       quantity: Noun.atom_binary_to_integer(quantity),
       isephemeral: Noun.Nounable.Bool.from_noun(ephemerality),
       nonce: Noun.atom_integer_to_binary(nonce, 32),
       nullifierkeycommitment:
         Noun.atom_integer_to_binary(nullifierkeycm, 32),
       randseed: Noun.atom_integer_to_binary(rseed)
     }}
  end
end
