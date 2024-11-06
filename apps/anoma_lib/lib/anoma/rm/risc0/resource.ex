defmodule Anoma.RM.Risc0.Resource do
  @moduledoc """
  I am a shielded resource for the RISC0 resource machine.
  """

  require Logger

  alias __MODULE__
  use TypedStruct

  alias Anoma.RM.Risc0.Constants

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
    # nullifier public key
    field(:npk, binary(), default: <<0::256>>)
    # random seed
    field(:rseed, binary(), default: <<0::256>>)
  end

  @doc "Randomizes the rseed of a resource."
  def random(r = %Resource{}) do
    rseed = :crypto.strong_rand_bytes(32)
    %Resource{r | rseed: rseed}
  end

  @doc """
  Set the nonce of a resource, the nonce of output resource comes from the
  nullifer of input recource in the compliance proof.
  """
  def set_nonce(r = %Resource{}, nonce) do
    %Resource{r | nonce: nonce}
  end

  @spec commitment(Resource.t()) :: binary()
  @doc "A commitment to the given resource."
  def commitment(resource = %Resource{}) do
    # TODO: consider if we can get rid of the psi from commitment
    psi =
      [
        Constants.zero(),
        resource.rseed,
        resource.nonce
      ]
      |> Enum.map(&:binary.bin_to_list/1)
      |> Risc0.sha256_many()
      |> :binary.list_to_bin()

    rcm =
      [
        Constants.one(),
        resource.rseed,
        resource.nonce
      ]
      |> Enum.map(&:binary.bin_to_list/1)
      |> Risc0.sha256_many()
      |> :binary.list_to_bin()

    eph_field =
      if resource.eph do
        Constants.one()
      else
        Constants.zero()
      end

    [
      resource.logic,
      resource.label,
      resource.data,
      resource.npk,
      resource.nonce,
      psi,
      resource.quantity,
      eph_field,
      rcm
    ]
    |> Enum.map(&:binary.bin_to_list/1)
    |> Risc0.sha256_many()
    |> :binary.list_to_bin()
  end

  @spec nullifier(Resource.t()) :: binary()
  @doc """
  The nullifier of the given resource.
  """
  def nullifier(resource = %Resource{}) do
    psi =
      [
        Constants.zero(),
        resource.rseed,
        resource.nonce
      ]
      |> Enum.map(&:binary.bin_to_list/1)
      |> Risc0.sha256_many()
      |> :binary.list_to_bin()

    [resource.npk, resource.nonce, psi, commitment(resource)]
    |> Enum.map(&:binary.bin_to_list/1)
    |> Risc0.sha256_many()
    |> :binary.list_to_bin()
  end

  @spec to_bytes(t()) :: [byte()]
  def to_bytes(resource = %__MODULE__{}) do
    binaries =
      resource.logic <>
        resource.label <>
        resource.quantity <>
        resource.data <>
        resource.nonce <> resource.npk <> resource.rseed

    binaries =
      if resource.eph do
        binaries <> <<1>>
      else
        binaries <> <<0>>
      end

    binaries |> :binary.bin_to_list()
  end

  @spec get_npk(binary()) :: binary()
  @doc """
  Generate the nullifier public key from the nulliffier (private)key.
  """
  def get_npk(nk) do
    Risc0.sha256_double(
      nk |> :binary.bin_to_list(),
      Constants.zero() |> :binary.bin_to_list()
    )
    |> :binary.list_to_bin()
  end
end
