defmodule CommitmentTreeTest do
  alias Examples.ECommitmentTree
  use TestHelper.TestMacro
  doctest CommitmentTree

  test "examples" do
    ECommitmentTree.sha256_32_spec()
    ECommitmentTree.memory_backed_ct()
    ECommitmentTree.empty_mnesia_backed_ct()
    ECommitmentTree.current_tree_mnesia_ct()
    ECommitmentTree.babylon_mnesia_ct()
    ECommitmentTree.current_tree_mnesia_ct()
    ECommitmentTree.lots_of_inserts_ct()
  end
end
