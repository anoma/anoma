defmodule Anoma.Identity.Verification do
  @moduledoc """

  I am responsible for verifying commitments made by external
  identities. I automatically uses "signs for" relationship
  information from the Anoma.Identity.SignsFor along with caller preference
  information in order to choose how to verify a commitment.
  """

  alias Anoma.Crypto.Id
  alias Anoma.Crypto.Sign
  alias Anoma.Storage
  alias Anoma.Identity.SignsFor

  @spec verify_request(binary(), term(), Id.Extern.t(), false | Storage.t()) ::
          boolean
  def verify_request(commitment, msg, id = %Id.Extern{}, signsFor \\ false) do
    bin_message = binary(msg)

    Sign.verify_detatched(commitment, bin_message, id.sign) ||
      (signsFor && signs_for_lookup(commitment, bin_message, id, signsFor))
  end

  # this should be the real verify function, new name until specs clarify
  @spec verify_combined(binary(), Id.Extern.t()) :: false | {:ok, any()}
  @spec verify_combined(binary(), Id.Extern.t(), false | Storage.t()) ::
          {:ok, term()} | false
  def verify_combined(commitment, id = %Id.Extern{}, signsFor \\ false) do
    {v, result} = Sign.verify(commitment, id.sign)

    if v == :ok do
      try do
        {:ok, :erlang.binary_to_term(result)}
      rescue
        _ in ArgumentError -> {:ok, result}
      end
    else
      signs_for_lookup(commitment, id, signsFor)
    end
  end

  # detached mode version of signs for
  @spec signs_for_lookup(
          binary(),
          binary(),
          Id.Extern.t(),
          false | Storage.t()
        ) :: boolean
  defp signs_for_lookup(commitment, bin_message, id, table) do
    table &&
      SignsFor.known(table, id)
      |> Enum.any?(&verify_request(commitment, bin_message, &1, table))
  end

  # This one is used when we have combined lookup only
  @spec signs_for_lookup(binary(), Id.Extern.t(), false | Storage.t()) ::
          boolean
  defp signs_for_lookup(blob, id, table) do
    table &&
      SignsFor.known(table, id)
      |> Enum.any?(&verify_combined(blob, &1, table))
  end

  @spec binary(term()) :: binary()
  defp binary(term) when is_binary(term), do: term
  defp binary(term), do: :erlang.term_to_binary(term)
end
