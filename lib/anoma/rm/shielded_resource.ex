defmodule Anoma.RM.ShieldedResource do
  @moduledoc """
  I am a shielded resource.
  """

  require Logger

  alias __MODULE__
  use TypedStruct

  alias Anoma.Constants

  typedstruct enforce: true do
    # resource logic
    field(:logic, binary(), default: <<>>)
    # fungibility label
    field(:label, binary(), default: <<>>)
    # quantity
    field(:quantity, binary(), default: <<>>)
    # arbitrary data
    field(:data, binary(), default: <<>>)
    # ephemerality flag
    field(:eph, bool(), default: false)
    # resource nonce
    field(:nonce, binary(), default: <<>>)
    # nullifier public key
    field(:npk, binary(), default: <<>>)
    # random seed
    field(:rseed, binary(), default: <<>>)
  end

  @doc "Randomizes the rseed of a resource."
  def random(r = %ShieldedResource{}) do
    rseed = :crypto.strong_rand_bytes(32)
    %ShieldedResource{r | rseed: rseed}
  end

  @doc """
  Set the nonce of a resource, the nonce of output resource comes from the
  nullifer of input recource in the compliance proof.
  """
  def set_nonce(r = %ShieldedResource{}, nonce) do
    %ShieldedResource{r | nonce: nonce}
  end

  @spec commitment(ShieldedResource.t()) :: binary()
  @doc "A commitment to the given resource."
  def commitment(resource = %ShieldedResource{}) do
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

    rcm =
      [
        Constants.prf_expand_personalization_felt(),
        Constants.felt_one(),
        resource.rseed,
        resource.nonce
      ]
      |> Enum.map(&:binary.bin_to_list/1)
      |> Cairo.poseidon_many()

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
      resource.npk,
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

  @spec nullifier(ShieldedResource.t()) :: binary()
  @doc """
  The nullifier of the given resource.
  """
  def nullifier(resource = %ShieldedResource{}) do
    psi =
      [
        Constants.prf_expand_personalization_felt(),
        Constants.felt_zero(),
        resource.rseed,
        resource.nonce
      ]
      |> Enum.map(&:binary.bin_to_list/1)
      |> Cairo.poseidon_many()

    [resource.npk, resource.nonce, psi, commitment(resource)]
    |> Enum.map(&:binary.bin_to_list/1)
    |> Cairo.poseidon_many()
    |> :binary.list_to_bin()
  end
end
