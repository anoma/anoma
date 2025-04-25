defmodule TransactionTest do
  use ExUnit.Case, async: true

  use TestHelper.GenerateExampleTests,
    for: Anoma.Controller.Examples.ETransaction
end
