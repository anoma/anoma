defmodule Anoma.Resource do
  @moduledoc """
  Ι represent a resource.

  Do not create with `%Anoma.Resource{}` directly, instead use
  `%{Anoma.Resource.new | ...}` for random nonce and seed.
  """

  require Logger

  alias __MODULE__
  use TypedStruct

  alias Anoma.Crypto.Sign

  typedstruct enforce: true do
    # resource logic
    field(:logic, Noun.t(), default: [[1 | 0], [0 | 0] | 0])
    # fungibility label
    field(:label, binary(), default: <<>>)
    # quantity
    field(:quantity, non_neg_integer(), default: 0)
    # arbitrary data
    field(:data, binary(), default: <<>>)
    # ephemerality flag
    field(:eph, bool(), default: false)
    # resource nonce
    field(:nonce, <<_::256>>, default: <<0::256>>)
    # nullifier public key
    field(:npk, Sign.ed25519_public(), default: <<0::256>>)
    # random seed
    field(:rseed, <<_::256>>, default: <<0::256>>)
  end

  @doc "New blank resource. Randomized nonce and seed."
  def new do
    nonce = :crypto.strong_rand_bytes(32)
    rseed = :crypto.strong_rand_bytes(32)
    %Resource{nonce: nonce, rseed: rseed}
  end

  @doc """
  Helper to pass in the npk for initializing a valid but meaningless
  resource.
  """
  def new_with_npk(npk) do
    %{new() | npk: npk}
  end

  @doc "A commitment to the given resource."
  def commitment(resource = %Resource{}) do
    "committo" <> :erlang.term_to_binary(resource)
  end

  @doc """
  The nullifier of the given resource.
  (It's up to the caller to use the right secret.)
  """
  def nullifier(resource = %Resource{}, secret) do
    ("annullo" <> :erlang.term_to_binary(resource))
    |> Sign.sign(secret)
  end

  @doc """
  The kind of the given resource (labelled logic).
  """
  def kind(resource = %Resource{}) do
    :erlang.term_to_binary(resource.logic) <>
      :erlang.term_to_binary(resource.label)
  end

  @doc """
  The delta of the given resource (kind and quantity).
  """
  def delta(resource = %Resource{}) do
    %{kind(resource) => resource.quantity}
  end

  def transparent_committed_resource(commitment) do
    with "committo" <> committed_resource_bytes <- commitment do
      {:ok, :erlang.binary_to_term(committed_resource_bytes)}
    else
      _ -> :error
    end
  end

  @doc """
  Whether a commitment commits to a given resource.
  """
  def commits_to(commitment, resource) do
    with {:ok, committed_resource} <-
           transparent_committed_resource(commitment) do
      committed_resource == resource
    else
      _ -> false
    end
  end

  def commits_to_any(commitment, resources) do
    Enum.any?(resources, fn r -> commitment |> commits_to(r) end)
  end

  @doc """
  Whether a nullifier nullifies a given resource.
  """
  def nullifies(nullifier, resource) do
    with {:ok, verified_nullifier} <- Sign.verify(nullifier, resource.npk),
         "annullo" <> nullified_resource_bytes <- verified_nullifier do
      :erlang.binary_to_term(nullified_resource_bytes) == resource
    else
      _ -> false
    end
  end

  def nullifies_any(nullifier, resources) do
    Enum.any?(resources, fn r -> nullifier |> nullifies(r) end)
  end

  def transparent_run_resource_logic(transaction, resource) do
    logic = resource.logic
    self = Anoma.Resource.to_noun(resource)
    tx = Anoma.Resource.Transaction.to_noun(transaction)
    arg = [self | tx]
    result = Nock.nock(logic, [9, 2, 10, [6, 1 | arg], 0 | 1])
    Logger.debug("resource logic nock result: #{inspect(result)}")

    case result do
      {:ok, 0} ->
        true

      _ ->
        false
    end
  end

  @doc """
  The resource as a noun.
  """
  @spec to_noun(t()) :: Noun.t()
  def to_noun(resource = %Resource{}) do
    [
      resource.logic,
      Noun.atom_binary_to_integer(resource.label),
      resource.quantity,
      Noun.atom_binary_to_integer(resource.data),
      if resource.eph do
        0
      else
        1
      end,
      Noun.atom_binary_to_integer(resource.nonce),
      Noun.atom_binary_to_integer(resource.npk)
      | Noun.atom_binary_to_integer(resource.rseed)
    ]
  end

  def from_noun([logic, label, quantity, data, eph, nonce, npk | rseed]) do
    %Resource{
      logic: logic,
      label: Noun.atom_integer_to_binary(label),
      quantity: quantity,
      data: Noun.atom_integer_to_binary(data),
      eph:
        case eph do
          0 -> true
          1 -> false
        end,
      nonce: Noun.atom_integer_to_binary(nonce),
      npk: Noun.atom_integer_to_binary(npk),
      rseed: Noun.atom_integer_to_binary(rseed)
    }
  end
end
