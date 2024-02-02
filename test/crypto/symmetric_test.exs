defmodule AnomaTest.Crypto.Symmetric do
  use ExUnit.Case, async: true
  alias Anoma.Crypto.Symmetric

  test "decrypt · encrypt ≡ identity" do
    xcc = Symmetric.random_xchacha()

    assert 555 ==
             555 |> Symmetric.encrypt(xcc) |> Symmetric.decrypt(xcc)
  end
end
