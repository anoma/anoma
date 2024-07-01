defmodule Examples.EIdentity do
  use Memoize

  require ExUnit.Assertions
  import ExUnit.Assertions

  alias Anoma.Identity.Verification
  alias Anoma.Node.Identity.Commitment
  alias Examples.ECrypto

  @spec alice_commits() :: GenServer.server()
  def alice_commits() do
    akey = ECrypto.alice()
    assert {:ok, cpid} = Commitment.start_link({akey.internal.sign, :ed25519})
    assert {:ok, data} = Commitment.commit(cpid, 555)

    assert Verification.verify_request(data, 555, akey.external),
           "should verify the same data"

    refute Verification.verify_request(data, <<555>>, akey.external),
           "Should not verify unrelated data"

    assert {:ok, data} = Commitment.commit_combined(cpid, <<55>>)

    assert {:ok, <<55>>} == Verification.verify_combined(data, akey.external),
           "should verify the same data coming in, and give back uncombined values"

    refute Verification.verify_combined(<<5>>, akey.external),
           "Fails on unrelated data"

    cpid
  end
end
