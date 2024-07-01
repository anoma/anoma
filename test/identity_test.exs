defmodule AnomaTest.Identity do
  alias Examples.EIdentity
  use TestHelper.TestMacro, async: true

  doctest(Anoma.Identity.Verification)
  doctest(Anoma.Node.Identity.Commitment)
  doctest(Anoma.Identity.Encryption)
  doctest(Anoma.Node.Identity.Decryption)

  test "examples" do
    EIdentity.alice_commits()
    EIdentity.alice_decrypts()
  end
end
