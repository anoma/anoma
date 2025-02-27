defmodule Anoma.CairoResource.Resource do
  @moduledoc """
  I am a shielded resource.
  """

  alias __MODULE__
  alias Anoma.CairoResource.Utils
  alias Anoma.Constants

  require Logger

  use TypedStruct

  typedstruct enforce: true do
    # resource logic
    field(:logic, <<_::256>>, default: <<0::256>>)
    # fungibility label
    field(:label, <<_::256>>, default: <<0::256>>)
    # quantity
    field(:quantity, <<_::256>>, default: <<0::256>>)
    # arbitrary data
    field(:data, <<_::256>>, default: <<0::256>>)
    # ephemerality flag
    field(:eph, bool(), default: false)
    # resource nonce
    field(:nonce, <<_::256>>, default: <<0::256>>)
    # commitment to nullifier key
    field(:nk_commitment, <<_::256>>, default: <<0::256>>)
    # random seed
    field(:rseed, <<_::256>>, default: <<0::256>>)
  end

  @spec from_json_object(Jason.OrderedObject.t()) ::
          {:ok, t()}
          | {:error, term()}
  def from_json_object(mp) do
    with {:ok, logic} <-
           Utils.parse_json_field_to_binary32(mp, "logic"),
         {:ok, label} <-
           Utils.parse_json_field_to_binary32(mp, "label"),
         {:ok, quantity} <-
           Utils.parse_json_field_to_binary32(mp, "quantity"),
         {:ok, data} <-
           Utils.parse_json_field_to_binary32(mp, "data"),
         {:ok, eph} <- Utils.parse_json_field_to_boolean(mp, "eph"),
         {:ok, nonce} <-
           Utils.parse_json_field_to_binary32(mp, "nonce"),
         {:ok, nk_commitment} <-
           Utils.parse_json_field_to_binary32(mp, "nk_commitment"),
         {:ok, rseed} <-
           Utils.parse_json_field_to_binary32(mp, "rseed") do
      %Resource{
        logic: logic,
        label: label,
        quantity: quantity,
        data: data,
        eph: eph,
        nonce: nonce,
        nk_commitment: nk_commitment,
        rseed: rseed
      }
    else
      {:error, msg} -> {:error, "Error parsing resource JSON: #{msg}"}
    end
  end

  @spec to_json_object(t()) :: Jason.OrderedObject.t()
  def to_json_object(resource) do
    %Jason.OrderedObject{
      values: [
        {"logic", Utils.binary_to_hex(resource.logic)},
        {"label", Utils.binary_to_hex(resource.label)},
        {"quantity", Utils.binary_to_hex(resource.quantity)},
        {"data", Utils.binary_to_hex(resource.data)},
        {"eph", resource.eph},
        {"nonce", Utils.binary_to_hex(resource.nonce)},
        {"nk_commitment", Utils.binary_to_hex(resource.nk_commitment)},
        {"rseed", Utils.binary_to_hex(resource.rseed)}
      ]
    }
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

  @spec nullifier(Resource.t(), binary()) :: binary()
  @doc """
  The nullifier of the given resource.
  """
  def nullifier(resource = %Resource{}, nk) do
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

    [nk, resource.nonce, psi, commitment(resource)]
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
