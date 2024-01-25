defmodule CommitmentTreeTest do
  use ExUnit.Case
  doctest CommitmentTree

  test "basic" do
    table = :cmtree_test
    CommitmentTree.init_storage(table)

    spec =
      CommitmentTree.Spec.new(32, 2, 256, fn {x, y} ->
        :crypto.hash(:sha256, x <> y)
      end)

    ct = CommitmentTree.new(spec, table)

    {ct, anchor} =
      CommitmentTree.add(
        ct,
        Enum.map(["abc", "abcc", "hiii", "moooo", "oo"], fn x ->
          :crypto.hash(:sha256, x)
        end)
      )

    # restoring from storage gives the same tree
    oct = CommitmentTree.new(spec, table)
    assert oct == ct

    # proofs
    hiii = :crypto.hash(:sha256, "hiii")

    assert CommitmentTree.Proof.verify(
             spec,
             CommitmentTree.prove(ct, 2),
             anchor,
             hiii
           )

    refute CommitmentTree.Proof.verify(
             spec,
             CommitmentTree.prove(ct, 3),
             anchor,
             hiii
           )

    :mnesia.delete_table(table)
  end

  test "random-insert" do
    # 100 times, insert 1-100 keys, and check the result is the same as adding them all at once, and adding them one at a time
    spec =
      CommitmentTree.Spec.new(32, 2, 256, fn x ->
        :crypto.hash(:sha256, elem(x, 0) <> elem(x, 1))
      end)

    ct = CommitmentTree.new(spec, nil)

    {ct_batches, keys} =
      Enum.reduce(1..100, {ct, []}, fn _, {ct, keys} ->
        new_keys =
          Enum.map(0..:rand.uniform(99), fn _ ->
            :crypto.strong_rand_bytes(32)
          end)

        {ct, _anchor} = CommitmentTree.add(ct, new_keys)
        {ct, keys ++ new_keys}
      end)

    {ct_allatonce, _anchor} = CommitmentTree.add(ct, keys)

    ct_oneatatime =
      Enum.reduce(keys, ct, fn cm, ct ->
        elem(CommitmentTree.add(ct, [cm]), 0)
      end)

    assert ct_batches == ct_allatonce
    assert ct_allatonce == ct_oneatatime
  end
end
