defmodule AnomaTest.Crypto do
  use ExUnit.Case, async: true

  alias Examples.ECrypto

  doctest(Anoma.Crypto.Id)

  test "examples" do
    ECrypto.xcc()
    ECrypto.londo()
  end
end
