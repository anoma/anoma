defmodule AnomaTest.Crypto.Id do
  use ExUnit.Case, async: true

  alias Anoma.Crypto.Id
  alias Anoma.Crypto.Symmetric

  doctest(Anoma.Crypto.Id)

  test "unsalt · salt ≡ identity" do
    xcc = Symmetric.random_xchacha()
    keys = Id.new_keypair()

    assert keys ==
             keys |> Id.salt_keys(xcc) |> Id.unsalt_keys(xcc)
  end
end
