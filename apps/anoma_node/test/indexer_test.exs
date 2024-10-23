defmodule IndexerTest do
  use ExUnit.Case, async: true
  use TestHelper.GenerateExampleTests, for: Anoma.Node.Examples.EIndexer
end
