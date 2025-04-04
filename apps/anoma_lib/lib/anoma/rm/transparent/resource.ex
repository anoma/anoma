defmodule Anoma.RM.Transparent.Resource do
  @moduledoc """
  I am the Resource module of the TRM.

  I provide the interface to interact and create resources for the
  transparent resource machine.

  ### Public API

  I provide the following public functionality

  - `commitment_hash/1`
  - `nullifier_hash/1`
  - `delta/1`
  - `kind/1`
  - `to_noun/1`
  - `from_noun/1`
  """
  alias __MODULE__
  alias Anoma.RM.Transparent.Primitive.DeltaHash

  use TypedStruct

  typedstruct enforce: true do
    # the jam of a resource logic
    field(:logicref, integer(),
      default:
        [[1 | 0], 0 | 0] |> Noun.Jam.jam() |> Noun.atom_binary_to_integer()
    )

    # jammed
    field(:labelref, integer(), default: 2)
    # jammed
    field(:valueref, integer(), default: 2)
    field(:quantity, integer(), default: 1)
    # whether the resource is ephemetal or not
    field(:isephemeral, boolean(), default: false)
    field(:nonce, <<_::256>>, default: <<0::256>>)
    # a commitment to the nullifier key
    # with the commitment hash being just the identity function
    field(:nullifierkeycommitment, Anoma.Crypto.Sign.public(),
      default: <<0::256>>
    )

    field(:randseed, <<>>, default: <<>>)
  end

  @doc """
  I am the commitment hash function for the TRM Resource.

  Given a resource, I turn it to a noun, jam it, and append the "CM_" tag
  onto it, afterwards turning to an integer.
  """
  @spec commitment_hash(t()) :: integer()
  def commitment_hash(resource) do
    binary_resource = resource |> Noun.Nounable.to_noun() |> Noun.Jam.jam()
    ("CM_" <> binary_resource) |> Noun.atom_binary_to_integer()
  end

  @doc """
  I am the nullifier hash function for the TRM Resource.

  Given a resource, I turn it to a noun, jam it, and append the "NF_" tag
  onto it, afterwards turning to an integer.
  """
  @spec nullifier_hash(<<_::256>>, t()) :: integer()
  def nullifier_hash(_nullifier_key, resource) do
    binary_resource = resource |> Noun.Nounable.to_noun() |> Noun.Jam.jam()
    ("NF_" <> binary_resource) |> Noun.atom_binary_to_integer()
  end

  @doc """
  I am the delta function of a transparent resource.

  I simply compute the kind of a resource, put it as a key with the value
  being the resource's quantity. Then I hash the preimage.
  """
  @spec delta(t()) :: integer()
  def delta(resource) do
    %{kind(resource) => resource.quantity}
    |> DeltaHash.hash()
  end

  @doc """
  I am the kind computation function for a transparent resource.

  I put the label and the logic in a cell, jam it and then hash it with a
  sha256.
  """
  @spec kind(t()) :: <<_::256>>
  def kind(%Resource{labelref: label, logicref: logic}) do
    kind = [label | logic] |> Noun.Jam.jam()
    :crypto.hash(:sha256, kind)
  end

  defimpl Noun.Nounable, for: Resource do
    @impl true
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
    with {:ok, boolean} <- Noun.Nounable.Bool.from_noun(ephemerality) do
      {:ok,
       %__MODULE__{
         logicref: Noun.atom_binary_to_integer(logicref),
         labelref: Noun.atom_binary_to_integer(labelref),
         valueref: Noun.atom_binary_to_integer(valueref),
         quantity: Noun.atom_binary_to_integer(quantity),
         isephemeral: boolean,
         nonce: Noun.atom_integer_to_binary(nonce, 32),
         nullifierkeycommitment:
           Noun.atom_integer_to_binary(nullifierkeycm, 32),
         randseed: Noun.atom_integer_to_binary(rseed)
       }}
    else
      _ -> :error
    end
  end

  @spec commits?(t(), Noun.noun_atom()) :: boolean()
  def commits?(self = %Resource{}, commitment) when is_binary(commitment) do
    commits?(self, Noun.atom_binary_to_integer(commitment))
  end

  def commits?(self = %Resource{}, commitment) when is_integer(commitment) do
    commitment_hash(self) == commitment
  end

  @spec nullifies?(t(), Noun.noun_atom()) :: boolean()
  def nullifies?(self = %Resource{}, nullifier) when is_binary(nullifier) do
    nullifies?(self, Noun.atom_binary_to_integer(nullifier))
  end

  def nullifies?(self = %Resource{}, nullifier) when is_integer(nullifier) do
    nullifier_hash(<<0::256>>, self) == nullifier
  end
end
