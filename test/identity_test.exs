defmodule AnomaTest.Identity do
  alias Examples.EIdentity
  use TestHelper.TestMacro, async: true

  doctest(Anoma.Identity.Verification)
  doctest(Anoma.Node.Identity.Commitment)

  test "examples" do
    EIdentity.alice_commits()
  end
end
