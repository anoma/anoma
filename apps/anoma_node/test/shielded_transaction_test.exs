defmodule Anoma.Node.ShieldedTransactionTest do
  use ExUnit.Case, async: true

  @moduletag :zk

  use TestHelper.GenerateExampleTests,
    for: Anoma.Node.Examples.EShieldedTransaction
end
