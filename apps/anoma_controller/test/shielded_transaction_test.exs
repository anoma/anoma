defmodule ShieldedTransactionTest do
  use ExUnit.Case, async: true

  @moduletag :zk

  use TestHelper.GenerateExampleTests,
    for: Anoma.Controller.Examples.EShieldedTransaction
end
