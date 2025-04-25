defmodule Anoma.Controller.Event.DefEventTest do
  use ExUnit.Case

  use TestHelper.GenerateExampleTests,
    for: Anoma.Controller.Examples.EEvent.EDefEvent
end
