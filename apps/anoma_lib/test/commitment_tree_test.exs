defmodule CommitmentTreeTest do
  alias Examples.ECommitmentTree
  use TestHelper.TestMacro
  doctest CommitmentTree

  test "sha256 examples" do
    ECommitmentTree.sha256_32_spec()
    ECommitmentTree.memory_backed_ct()
    ECommitmentTree.empty_mnesia_backed_ct()
    ECommitmentTree.current_tree_mnesia_ct()
    ECommitmentTree.babylon_mnesia_ct()
    ECommitmentTree.current_tree_mnesia_ct()
    ECommitmentTree.lots_of_inserts_ct()
  end

  test "cairo poseidon examples" do
    cairo_spec = ECommitmentTree.cairo_poseidon_spec()
    ECommitmentTree.memory_backed_ct(cairo_spec)
    ECommitmentTree.empty_mnesia_backed_ct(cairo_spec)
    ECommitmentTree.current_tree_mnesia_ct(cairo_spec)
    ECommitmentTree.babylon_mnesia_ct(cairo_spec)
    ECommitmentTree.current_tree_mnesia_ct(cairo_spec)
    ECommitmentTree.lots_of_inserts_ct(cairo_spec)
  end
end
