defmodule TransactionTest do
  use ExUnit.Case, async: true
  use TestHelper.GenerateExampleTests, for: Anoma.Node.Examples.ETransaction

  use TestHelper.GenerateExampleTests,
    for: Anoma.Node.Examples.EShieldedTransaction
end
