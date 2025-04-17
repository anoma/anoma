defmodule Examples.PubSubTest do
  use ExUnit.Case, async: false
  use TestHelper.TestMacro

  use TestHelper.GenerateExampleTests,
    for: Anoma.Node.Examples.EPubSub
end
