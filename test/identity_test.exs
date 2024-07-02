defmodule AnomaTest.Identity do
  alias Examples.EIdentity
  use TestHelper.TestMacro, async: true

  doctest(Anoma.Identity.Verification)
  doctest(Anoma.Node.Identity.Commitment)
  doctest(Anoma.Identity.Encryption)
  doctest(Anoma.Node.Identity.Decryption)
  doctest(Anoma.Identity.Name)
  doctest(Anoma.Identity.SignsFor)

  test "examples" do
    EIdentity.alice_commits()
    EIdentity.alice_decrypts()
    EIdentity.failure_to_connect()
    EIdentity.memory_storage()
    EIdentity.memory_storage_connected_engines()
    EIdentity.no_memory_storage()
    EIdentity.same_id_multiple_times()
  end
end
