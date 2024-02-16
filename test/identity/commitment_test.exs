defmodule AnomaTest.Identity.Commitment do
  use ExUnit.Case, async: true

  alias Anoma.Node.Identity.Commitment
  alias Anoma.Crypto.Id
  alias Anoma.Identity.Verification

  doctest(Anoma.Identity.Verification)
  doctest(Anoma.Node.Identity.Commitment)

  test "Basic verification works" do
    pair = Id.new_keypair()
    encapsulated = {pair.internal.sign, :ed25519}
    {:ok, cpid} = Commitment.start_link(encapsulated)

    {:ok, data} = Commitment.commit(cpid, 555)
    assert Verification.verify_request(data, pair.external)
  end

  test "Verifying fails on unrelated data" do
    pair = Id.new_keypair()
    refute Verification.verify_request(<<5>>, pair.external)
  end
end
