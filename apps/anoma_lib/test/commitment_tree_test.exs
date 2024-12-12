defmodule CommitmentTreeTest do
  alias Examples.ECommitmentTree

  use TestHelper.TestMacro

  use TestHelper.GenerateExampleTests,
    for: ECommitmentTree

  doctest CommitmentTree
end
