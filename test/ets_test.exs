defmodule SparseMerkleTreeTest.ETS do
  use TestHelper.TestMacro

  use TestHelper.GenerateExampleTests,
    for: Examples.ESparseMerkleTree.EETS

  doctest SparseMerkleTree.ETS
end
