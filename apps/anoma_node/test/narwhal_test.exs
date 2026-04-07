defmodule NarwhalTest do
  use ExUnit.Case, async: true

  use TestHelper.GenerateExampleTests, for: Anoma.Node.Examples.ENarwhal
end
