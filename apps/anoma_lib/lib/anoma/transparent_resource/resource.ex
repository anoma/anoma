defmodule Anoma.TransparentResource.Resource do
  @moduledoc """
  resource struct
  """

  use TypedStruct
  alias __MODULE__

  @type commitment() :: binary()
  @type nullifier() :: binary()

  typedstruct enforce: true do
    # following two are together the kind
    field(:label, binary(), default: "")
    field(:logic, Noun.t(), default: [[1 | 0], 0 | 0])
    # ephemerality flag
    field(:ephemeral, bool(), default: false)
    # following are more value-like fields
    field(:quantity, non_neg_integer(), default: 1)
    # This field is NOT named "value".
    # The phrase "The value of resource R" means something.
    # What it means is:
    # - The value of the resource's label field, and
    # - the value of the resource's logic field, and
    # - the value of the resource's ephemerality flag field, and
    # - the value of the resource's quantity field, and
    # - the value of the resource's data field, and
    # - the value of the resource's nullifier key commitment field, and
    # - the value of the resource's nonce field, and
    # - the value of the resource's random seed field,
    # all together.
    field(:data, binary(), default: <<>>)
    # nullifier key. should be a public-key like thing probably
    field(:nullifier_key, Anoma.Crypto.Sign.public(), default: <<0::256>>)
    # for uniqueness
    field(:nonce, <<_::256>>, default: <<0::256>>)
    # useless field for shielded only.
    field(:rseed, <<>>, default: <<>>)
  end

  def to_noun(resource = %Resource{}) do
    [
      resource.label,
      resource.logic,
      bool_to_noun(resource.ephemeral),
      resource.quantity,
      resource.data,
      resource.nullifier_key,
      resource.nonce,
      resource.rseed
    ]
  end

  def from_noun([
        label,
        logic,
        ephemeral,
        quantity,
        data,
        nullifier_key,
        nonce,
        rseed | terminator
      ])
      when terminator in [0, <<>>, <<0>>, []] do
    # we make sure the types are respected
    {:ok,
     %Resource{
       label: Noun.atom_integer_to_binary(label),
       logic: logic,
       ephemeral: noun_to_bool(ephemeral),
       quantity: Noun.atom_binary_to_integer(quantity),
       data: Noun.atom_integer_to_binary(data),
       nullifier_key: Noun.atom_integer_to_binary(nullifier_key, 32),
       nonce: Noun.atom_integer_to_binary(nonce, 32),
       rseed: Noun.atom_integer_to_binary(rseed)
     }}
  end

  def from_noun(_) do
    :error
  end

  def noun_to_bool(zero) when zero in [0, <<>>, <<0>>, []] do
    true
  end

  def noun_to_bool(one) when one in [1, <<1>>] do
    false
  end

  def bool_to_noun(true) do
    0
  end

  def bool_to_noun(false) do
    1
  end

  def kind(%Resource{label: label, logic: logic}) do
    kind = label <> Noun.atom_integer_to_binary(Nock.Jam.jam(logic))
    :crypto.hash(:sha256, kind)
  end

  def delta(resource = %Resource{}) do
    %{kind(resource) => resource.quantity}
  end

  def commitment(resource = %Resource{}) do
    binary_resource = resource |> to_noun() |> Nock.Jam.jam()
    "CM_" <> binary_resource
  end

  def nullifier(resource = %Resource{}) do
    binary_resource = resource |> to_noun() |> Nock.Jam.jam()
    "NF_" <> binary_resource
  end

  @spec commits?(t(), Noun.noun_atom()) :: boolean()
  def commits?(self = %Resource{}, commitment) when is_binary(commitment) do
    commitment(self) == commitment
  end

  def commits?(self = %Resource{}, commitment) when is_integer(commitment) do
    commits?(self, Noun.atom_integer_to_binary(commitment))
  end

  @spec nullifies?(t(), Noun.noun_atom()) :: boolean()
  def nullifies?(self = %Resource{}, nullifier) when is_binary(nullifier) do
    nullifier(self) == nullifier
  end

  def nullifies?(self = %Resource{}, nullifier) when is_integer(nullifier) do
    nullifies?(self, Noun.atom_integer_to_binary(nullifier))
  end
end
