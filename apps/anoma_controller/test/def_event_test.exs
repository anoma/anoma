defmodule Anoma.Node.Event.DefEventTest do
  use ExUnit.Case

  use TestHelper.GenerateExampleTests,
    for: Anoma.Node.Examples.EEvent.EDefEvent
end
