defmodule RocksBackingTest do
  use ExUnit.Case, async: true

  use TestHelper.GenerateExampleTests,
    for: Anoma.Node.Examples.EReplay.RocksBacking
end
