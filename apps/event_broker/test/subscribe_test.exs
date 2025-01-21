defmodule EventbrokerTest.SubscribeTest do
  use ExUnit.Case, async: true

  use TestHelper.GenerateExampleTests,
    for: Examples.EEventBroker.Subscribe
end
