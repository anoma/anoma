defmodule Anoma.TransparentResource.Resource do
  @moduledoc """
  resource struct
  """

  use TypedStruct
  alias __MODULE__

  typedstruct enforce: true do
    # following two are together the kind
    field(:label, binary(), default: "")
    field(:logic, Noun.t(), default: [[1 | 0], 0 | 0])
    # ephemerality flag
    field(:ephemeral, bool(), default: false)
    # following are more value-like fields
    field(:quantity, integer(), default: 1)
    # MUST rename. "value of a resource" means something!
    field(:value, binary(), default: <<>>)
    # nullifier key. should be a public-key like thing probably
    field(:nullifier_key, <<_::256>>, default: <<0::256>>)
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
      resource.value,
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
        value,
        nullifier_key,
        nonce,
        rseed | terminator
      ])
      when terminator in [0, <<>>, <<0>>, []] do
    {:ok,
     %Resource{
       label: label,
       logic: logic,
       ephemeral: noun_to_bool(ephemeral),
       quantity: quantity,
       value: value,
       nullifier_key: nullifier_key,
       nonce: nonce,
       rseed: rseed
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

  def commitment(resource = %Resource{}) do
    binary_resource = resource |> to_noun() |> Nock.Jam.jam()
    :crypto.hash(:sha256, "committo" <> binary_resource)
  end

  def nullifier(resource = %Resource{}) do
    binary_resource = resource |> to_noun() |> Nock.Jam.jam()
    :crypto.hash(:sha256, "annullo" <> binary_resource)
  end
end
