defmodule Anoma.CairoResource.Resource do
  @moduledoc """
  I am a shielded resource.
  """

  alias Anoma.CairoResource.Utils

  require Logger

  alias __MODULE__
  use TypedStruct

  alias Anoma.Constants

  typedstruct enforce: true do
    # Hash of the predicate associated with the resource (resource logic)
    field(:logic_ref, <<_::256>>, default: <<0::256>>)
    # Hash of the resource label
    field(:label_ref, <<_::256>>, default: <<0::256>>)
    # quantity
    field(:quantity, <<_::256>>, default: <<0::256>>)
    # Hash of the resource value
    field(:value_ref, <<_::256>>, default: <<0::256>>)
    # ephemerality flag
    field(:is_ephemeral, bool(), default: false)
    # resource nonce
    field(:nonce, <<_::256>>, default: <<0::256>>)
    # commitment to nullifier key
    field(:nk_commitment, <<_::256>>, default: <<0::256>>)
    # random seed
    field(:rand_seed, <<_::256>>, default: <<0::256>>)
  end

  @spec from_json_object(Jason.OrderedObject.t()) ::
          {:ok, t()}
          | {:error, term()}
  def from_json_object(mp) do
    with {:ok, logic_ref} <-
           Utils.parse_json_field_to_binary32(mp, "logic_ref"),
         {:ok, label_ref} <-
           Utils.parse_json_field_to_binary32(mp, "label_ref"),
         {:ok, quantity} <-
           Utils.parse_json_field_to_binary32(mp, "quantity"),
         {:ok, value_ref} <-
           Utils.parse_json_field_to_binary32(mp, "value_ref"),
         {:ok, is_ephemeral} <-
           Utils.parse_json_field_to_boolean(mp, "is_ephemeral"),
         {:ok, nonce} <-
           Utils.parse_json_field_to_binary32(mp, "nonce"),
         {:ok, nk_commitment} <-
           Utils.parse_json_field_to_binary32(mp, "nk_commitment"),
         {:ok, rand_seed} <-
           Utils.parse_json_field_to_binary32(mp, "rand_seed") do
      %Resource{
        logic_ref: logic_ref,
        label_ref: label_ref,
        quantity: quantity,
        value_ref: value_ref,
        is_ephemeral: is_ephemeral,
        nonce: nonce,
        nk_commitment: nk_commitment,
        rand_seed: rand_seed
      }
    else
      {:error, msg} -> {:error, "Error parsing resource JSON: #{msg}"}
    end
  end

  @spec to_json_object(t()) :: Jason.OrderedObject.t()
  def to_json_object(resource) do
    %Jason.OrderedObject{
      values: [
        {"logic_ref", Utils.binary_to_hex(resource.logic_ref)},
        {"label_ref", Utils.binary_to_hex(resource.label_ref)},
        {"quantity", Utils.binary_to_hex(resource.quantity)},
        {"value_ref", Utils.binary_to_hex(resource.value_ref)},
        {"is_ephemeral", resource.is_ephemeral},
        {"nonce", Utils.binary_to_hex(resource.nonce)},
        {"nk_commitment", Utils.binary_to_hex(resource.nk_commitment)},
        {"rand_seed", Utils.binary_to_hex(resource.rand_seed)}
      ]
    }
  end

  @doc "Randomizes the rand_seed of a resource."
  @spec random(t()) :: t()
  def random(r = %Resource{}) do
    rand_seed = :crypto.strong_rand_bytes(32)
    %Resource{r | rand_seed: rand_seed}
  end

  @doc """
  Set the nonce of a resource, the nonce of output resource comes from the
  nullifer of input recource in the compliance proof.
  """
  @spec set_nonce(t(), binary()) :: t()
  def set_nonce(r = %Resource{}, nonce) do
    %Resource{r | nonce: nonce}
  end

  @spec commitment(Resource.t()) :: binary()
  @doc "A commitment to the given resource."
  def commitment(resource = %Resource{}) do
    # TODO: consider if we can get rid of the psi from commitment
    psi =
      [
        Constants.prf_expand_personalization_felt(),
        Constants.felt_zero(),
        resource.rand_seed,
        resource.nonce
      ]
      |> Enum.map(&:binary.bin_to_list/1)
      |> Cairo.poseidon_many()
      |> :binary.list_to_bin()

    rcm =
      [
        Constants.prf_expand_personalization_felt(),
        Constants.felt_one(),
        resource.rand_seed,
        resource.nonce
      ]
      |> Enum.map(&:binary.bin_to_list/1)
      |> Cairo.poseidon_many()
      |> :binary.list_to_bin()

    eph_field =
      if resource.is_ephemeral do
        Constants.felt_one()
      else
        Constants.felt_zero()
      end

    [
      resource.logic_ref,
      resource.label_ref,
      resource.value_ref,
      resource.nk_commitment,
      resource.nonce,
      psi,
      resource.quantity,
      eph_field,
      rcm
    ]
    |> Enum.map(&:binary.bin_to_list/1)
    |> Cairo.poseidon_many()
    |> :binary.list_to_bin()
  end

  @spec nullifier(Resource.t(), binary()) :: binary()
  @doc """
  The nullifier of the given resource.
  """
  def nullifier(resource = %Resource{}, nk) do
    psi =
      [
        Constants.prf_expand_personalization_felt(),
        Constants.felt_zero(),
        resource.rand_seed,
        resource.nonce
      ]
      |> Enum.map(&:binary.bin_to_list/1)
      |> Cairo.poseidon_many()
      |> :binary.list_to_bin()

    [nk, resource.nonce, psi, commitment(resource)]
    |> Enum.map(&:binary.bin_to_list/1)
    |> Cairo.poseidon_many()
    |> :binary.list_to_bin()
  end

  @spec to_bytes(t()) :: [byte()]
  def to_bytes(resource = %Resource{}) do
    binaries =
      resource.logic_ref <>
        resource.label_ref <>
        resource.quantity <>
        resource.value_ref <>
        resource.nonce <> resource.nk_commitment <> resource.rand_seed

    binaries =
      if resource.is_ephemeral do
        binaries <> <<1>>
      else
        binaries <> <<0>>
      end

    binaries |> :binary.bin_to_list()
  end

  @spec get_nk_commitment(binary()) :: binary()
  @doc """
  Generate the nullifier key commitment from the nulliffier key.
  """
  def get_nk_commitment(nk) do
    Cairo.poseidon(
      nk |> :binary.bin_to_list(),
      Constants.felt_zero() |> :binary.bin_to_list()
    )
    |> :binary.list_to_bin()
  end
end
