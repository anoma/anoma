defmodule SparseMerkleTreeTest.Naive do
  use TestHelper.TestMacro

  use TestHelper.GenerateExampleTests,
    for: Examples.ESparseMerkleTree.ENaive

  doctest SparseMerkleTree.Naive
end
