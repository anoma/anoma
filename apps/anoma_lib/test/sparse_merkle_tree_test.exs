defmodule SparseMerkleTreeTest do
  use TestHelper.TestMacro
  use TestHelper.GenerateExampleTests,
      for: Examples.ESparseMerkleTree

  doctest SparseMerkleTree
end