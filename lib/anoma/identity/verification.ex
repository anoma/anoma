defmodule Anoma.Identity.Verification do
  @moduledoc """

  I am responsible for verifying commitments made by external
  identities. I automatically uses "signs for" relationship
  information from the Anoma.Identity.SignsFor along with caller preference
  information in order to choose how to verify a commitment.
  """

  alias Anoma.Crypto.Id
  alias Anoma.Crypto.Sign

  @spec verify_request(binary(), Id.Extern.t(), boolean) :: boolean
  def verify_request(commitment, id = %Id.Extern{}, signsFor \\ false) do
    {v, _result} = Sign.verify(commitment, id.sign)
    v == :ok || (signsFor && signs_for_lookup(commitment, id, signsFor))
  end

  @spec signs_for_lookup(binary(), Id.Extern.t(), any()) :: boolean
  defp signs_for_lookup(_commitment, _id, _table) do
    false
  end
end
