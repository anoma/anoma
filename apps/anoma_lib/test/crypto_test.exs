defmodule AnomaTest.Crypto do
  alias Anoma.Examples

  use ExUnit.Case, async: true
  use TestHelper.GenerateExampleTests, for: Examples.ECrypto

  doctest(Anoma.Crypto.Id)
end
