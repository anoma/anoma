defmodule Anoma.Node.LoggingTest do
  use ExUnit.Case, async: true

  use TestHelper.GenerateExampleTests,
    for: Anoma.Node.Examples.ELogging
end
