defmodule LoggingTest do
  use ExUnit.Case, async: false

  use TestHelper.GenerateExampleTests,
    for: Anoma.Node.Examples.ELogging
end
