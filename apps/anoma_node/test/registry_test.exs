defmodule Anoma.Node.RegistryTest do
  use ExUnit.Case, async: true
  use TestHelper.TestMacro
  use TestHelper.GenerateExampleTests, for: Anoma.Node.Examples.ERegistry
end
