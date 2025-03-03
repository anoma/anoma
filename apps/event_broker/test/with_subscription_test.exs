defmodule EventbrokerTest.WithSub do
  use ExUnit.Case, async: true

  use TestHelper.GenerateExampleTests,
    for: Examples.EEVentBroker.WithSub
end
