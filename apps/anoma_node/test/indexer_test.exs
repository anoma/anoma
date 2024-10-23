defmodule IndexerTest do
  use ExUnit.Case, async: false
  use TestHelper.GenerateExampleTests, for: Anoma.Node.Examples.EIndexer
end
