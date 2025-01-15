defmodule SparseMerkleTreeTest do
  @moduledoc false

  use TestHelper.TestMacro, async: true
  use TestHelper.GenerateExampleTests,
    for: Examples.ESparseMerkleTree
end
