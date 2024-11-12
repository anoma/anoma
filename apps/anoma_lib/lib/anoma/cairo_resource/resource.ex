defmodule Anoma.CairoResource.Resource do
  @moduledoc """
  I am a shielded resource.
  """

  require Logger

  alias __MODULE__
  use TypedStruct

  alias Anoma.Constants

  typedstruct enforce: true do
    # resource logic
    field(:logic, binary(), default: <<0::256>>)
    # fungibility label
    field(:label, binary(), default: <<0::256>>)
    # quantity
    field(:quantity, binary(), default: <<0::256>>)
    # arbitrary data
    field(:data, binary(), default: <<0::256>>)
    # ephemerality flag
    field(:eph, bool(), default: false)
    # resource nonce
    field(:nonce, binary(), default: <<0::256>>)
    # commitment to nullifier key
    field(:nk_commitment, binary(), default: <<0::256>>)
    # random seed
    field(:rseed, binary(), default: <<0::256>>)
  end

  @doc "Randomizes the rseed of a resource."
  @spec random(t()) :: t()
  def random(r = %Resource{}) do
    rseed = :crypto.strong_rand_bytes(32)
    %Resource{r | rseed: rseed}
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
        resource.rseed,
        resource.nonce
      ]
      |> Enum.map(&:binary.bin_to_list/1)
      |> Cairo.poseidon_many()
      |> :binary.list_to_bin()

    rcm =
      [
        Constants.prf_expand_personalization_felt(),
        Constants.felt_one(),
        resource.rseed,
        resource.nonce
      ]
      |> Enum.map(&:binary.bin_to_list/1)
      |> Cairo.poseidon_many()
      |> :binary.list_to_bin()

    eph_field =
      if resource.eph do
        Constants.felt_one()
      else
        Constants.felt_zero()
      end

    [
      resource.logic,
      resource.label,
      resource.data,
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

  @spec nullifier(Resource.t()) :: binary()
  @doc """
  The nullifier of the given resource.
  """
  def nullifier(resource = %Resource{}) do
    psi =
      [
        Constants.prf_expand_personalization_felt(),
        Constants.felt_zero(),
        resource.rseed,
        resource.nonce
      ]
      |> Enum.map(&:binary.bin_to_list/1)
      |> Cairo.poseidon_many()
      |> :binary.list_to_bin()

    [resource.nk_commitment, resource.nonce, psi, commitment(resource)]
    |> Enum.map(&:binary.bin_to_list/1)
    |> Cairo.poseidon_many()
    |> :binary.list_to_bin()
  end

  @spec to_bytes(t()) :: [byte()]
  def to_bytes(resource = %Resource{}) do
    binaries =
      resource.logic <>
        resource.label <>
        resource.quantity <>
        resource.data <>
        resource.nonce <> resource.nk_commitment <> resource.rseed

    binaries =
      if resource.eph do
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
