defmodule CommitmentTreeTest do
  use ExUnit.Case, async: true
  alias Examples.ECommitmentTree

  use TestHelper.TestMacro

  use TestHelper.GenerateExampleTests,
    for: ECommitmentTree

  doctest CommitmentTree

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
