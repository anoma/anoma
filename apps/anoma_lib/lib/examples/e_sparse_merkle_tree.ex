defmodule Examples.ESparseMerkleTree do
  import SparseMerkleTree
  import ExUnit.Assertions

  use Memoize

  @data_a "data_a"
  @data_b "data_b"
  @data_c "abcdefgh"
  @data_h "data_h"
  @empty_hash <<0::256>>
  @data_hash_b SparseMerkleTree.hash(@data_b)
  @branch_hash_b3 SparseMerkleTree.hash(@empty_hash <> @data_hash_b)
  @data_hash_a SparseMerkleTree.hash(@data_a)
  @branch_hash_a3 SparseMerkleTree.hash(@data_hash_a <> @empty_hash)
  @branch_hash_a_b1 SparseMerkleTree.hash(@branch_hash_b3 <> @branch_hash_a3)
  @branch_hash_a_b SparseMerkleTree.hash(@branch_hash_a_b1 <> @empty_hash)
  @branch_hash_a2 SparseMerkleTree.hash(@empty_hash <> @branch_hash_a3)
  @branch_hash_a1 SparseMerkleTree.hash(@branch_hash_a2 <> @empty_hash)
  @branch_hash_b2 SparseMerkleTree.hash(@branch_hash_b3 <> @empty_hash)
  @branch_hash_b1 SparseMerkleTree.hash(@branch_hash_b2 <> @empty_hash)

  def empty_tree() do
    new()
  end

  def abc_tree() do
    empty_tree() |> insert("abc")
  end

  def def_tree() do
    empty_tree() |> insert("def")
  end

  def abc_def_tree() do
    empty_tree() |> insert("abc") |> insert("def")
  end

  def def_abc_tree() do
    empty_tree() |> insert("def") |> insert("abc")
  end

  def abc_def_equals_def_abc() do
    abc_def = abc_def_tree()
    ^abc_def = def_abc_tree()
  end

  def safe_double_insert() do
    abc = abc_tree()
    ^abc = insert(abc, "abc")
  end

  def prove_abc_present_in_abc() do
    tree = abc_tree()
    proof = {true, _} = prove(tree, "abc")
    true = verify(root(tree), "abc", proof)
  end

  def prove_abc_present_in_abc_def() do
    tree = abc_def_tree()
    proof = {true, _} = prove(tree, "abc")
    true = verify(root(tree), "abc", proof)
  end

  def prove_abc_absent_in_def() do
    tree = def_tree()
    proof = {false, _} = prove(tree, "abc")
    true = verify(root(tree), "abc", proof)
  end

  def prove_abc_absent_in_empty() do
    tree = empty_tree()
    proof = {false, _} = prove(tree, "abc")
    true = verify(root(tree), "abc", proof)
  end

  defmemo big_tree() do
    for n <- 1..(2 ** 16), reduce: new() do
      tree ->
        tree |> insert(Integer.to_string(n))
    end
  end

  def full_depth_tree() do
    tree = SparseMerkleTree.new()
    first = "first"
    second = "second"
    zero = "zero"

    assert tree = SparseMerkleTree.insert(tree, first)
    assert ^tree = SparseMerkleTree.insert(tree, first)

    assert tree = SparseMerkleTree.insert(tree, second)

    assert tree =
      SparseMerkleTree.insert(tree, zero)

    assert {root_hash, depth} = SparseMerkleTree.root(tree)

    assert {true, first_proof} =
             SparseMerkleTree.prove(tree, first)

    assert Enum.count(first_proof) == 256

    assert SparseMerkleTree.verify(
      {root_hash, depth},
      first,
      {true, first_proof}
    )

    refute SparseMerkleTree.verify(
      {root_hash, depth},
      first,
      {false, first_proof}
    )

    other = "other"

    assert {false, other_proof} =
             SparseMerkleTree.prove(tree, other)

    assert Enum.count(other_proof) == 3

    assert_raise RuntimeError, fn ->
      SparseMerkleTree.verify(
        {root_hash, depth},
        other,
        {true, other_proof}
      )
    end

    assert SparseMerkleTree.verify(
      {root_hash, depth},
      other,
      {false, other_proof}
    )

    tree
  end

  defp tree_depth_3() do
    SparseMerkleTree.new(depth: 3)
  end

  defp tree_depth_3_with_a() do
    tree = tree_depth_3()
    assert tree = SparseMerkleTree.insert(tree, @data_a)
    tree
  end

  defp tree_depth_3_with_a_rehash_b() do
    tree = tree_depth_3_with_a()

    assert tree = SparseMerkleTree.insert(tree, @data_b)
    tree
  end

  defp tree_depth_3_with_a_b_rehash() do
    tree = tree_depth_3_with_a()
    assert tree = SparseMerkleTree.insert(tree, @data_b)
    tree
  end

  def tree_depth_3_structure() do
    tree = tree_depth_3()

    assert %SparseMerkleTree{
             digests: %{},
             depth: 3
           } == tree

    tree
  end

  def tree_depth_3_rehashing_does_nothing() do
    tree_depth_3()
  end

  def tree_depth_3_proofs() do
    tree = tree_depth_3()
    root_hash = SparseMerkleTree.root(tree)

    assert {false, []} ==
      SparseMerkleTree.prove(tree, @data_a)

    assert {false, []} ==
      SparseMerkleTree.prove(tree, @data_b)

    assert SparseMerkleTree.verify(
      root_hash,
      @data_a,
      {false, []}
    )

    assert SparseMerkleTree.verify(
      root_hash,
      @data_b,
      {false, []}
    )

    tree
  end

  def tree_depth_3_drop() do
    tree = tree_depth_3()

    assert tree ==
      SparseMerkleTree.remove(tree, @data_a)

    assert tree ==
      SparseMerkleTree.remove(tree, @data_b)

    tree
  end

  @spec tree_depth_3_with_a_structure() :: SparseMerkleTree.t()
  def tree_depth_3_with_a_structure() do
    tree = tree_depth_3_with_a()

    assert %{
      <<>> => @branch_hash_a1,
      <<0::1>> => @branch_hash_a2,
      <<1::1, 0::1>> => @branch_hash_a3,
      <<0::1, 1::1, 0::1>> => @data_hash_a
    } ==
      tree.digests

    tree
  end

  def tree_depth_3_with_a_drop() do
    tree = tree_depth_3_with_a()

    assert tree ==
      SparseMerkleTree.remove(tree, @data_b)

    assert tree =
      SparseMerkleTree.remove(tree, @data_a)

    assert %{} == tree.digests
    tree
  end

  def tree_depth_3_with_a_rehash_b_structure() do
    tree = tree_depth_3_with_a_rehash_b()
    
    assert %{
    <<>> => @branch_hash_a_b,
    <<0::1>> => @branch_hash_a_b1,
    <<1::1, 0::1>> => @branch_hash_a3,
    <<0::1, 1::1, 0::1>> => @data_hash_a,
    <<0::1, 0::1>> => @branch_hash_b3,
    <<1::1, 0::1, 0::1>> => @data_hash_b
    } ==
      tree.digests

    tree
  end

  def tree_depth_3_with_a_rehash_b_duplicate_insert() do
    tree = tree_depth_3_with_a_rehash_b()

    assert ^tree =
      SparseMerkleTree.insert(tree, @data_b)

    tree
  end

  def tree_depth_3_with_a_rehash_b_root_hash() do
    tree = tree_depth_3_with_a_rehash_b()
    assert {@branch_hash_a_b, _depth} = SparseMerkleTree.root(tree)
    tree
  end

  def tree_depth_3_with_a_rehash_b_data_hash_collision() do
    tree = tree_depth_3_with_a_rehash_b()

    # Due to the very shallow depth of the tree, the next value's path
    # collides with the second. This should not happen with default-height
    # trees.
    assert_raise RuntimeError, fn -> SparseMerkleTree.insert(tree, @data_c) end

    tree
  end

  def tree_depth_3_with_a_rehash_b_drop() do
    tree = tree_depth_3_with_a_rehash_b()

    assert_raise RuntimeError, fn -> SparseMerkleTree.remove(tree, @data_c) end

    assert tree =
      SparseMerkleTree.remove(tree, @data_a)

    assert %{
    <<>> => @branch_hash_b1,
    <<0::1>> => @branch_hash_b2,
    <<0::1, 0::1>> => @branch_hash_b3,
    <<1::1, 0::1, 0::1>> => @data_hash_b
    } ==
      tree.digests

    assert tree =
      SparseMerkleTree.remove(tree, @data_b)

    assert %{} == tree.digests
    tree
  end

  def tree_depth_3_with_a_b_rehash_structure() do
    tree = tree_depth_3_with_a_b_rehash()

    assert %{
    <<>> => @branch_hash_a_b,
    <<0::1>> => @branch_hash_a_b1,
    <<1::1, 0::1>> => @branch_hash_a3,
    <<0::1, 1::1, 0::1>> => @data_hash_a,
    <<0::1, 0::1>> => @branch_hash_b3,
    <<1::1, 0::1, 0::1>> => @data_hash_b
    } ==
      tree.digests

    tree
  end

  def tree_depth_3_with_a_b_rehash_root_hash() do
    tree = tree_depth_3_with_a_b_rehash()
    assert {@branch_hash_a_b, _depth} = SparseMerkleTree.root(tree)
    tree
  end

  def tree_depth_3_with_a_b_rehash_proofs() do
    tree = tree_depth_3_with_a_b_rehash()
    root_hash = SparseMerkleTree.root(tree)
    valid_proof? = &SparseMerkleTree.verify(root_hash, &1, &2)

    a_proof = {true, [@empty_hash, @branch_hash_b3, @empty_hash]}
    b_proof = {true, [@empty_hash, @branch_hash_a3, @empty_hash]}
    h_proof = {false, [@data_hash_b, @branch_hash_a3, @empty_hash]}
    empty_proof = {false, [@branch_hash_a1, @empty_hash, @data_hash_b]}

    assert a_proof == SparseMerkleTree.prove(tree, @data_a)
    assert b_proof == SparseMerkleTree.prove(tree, @data_b)
    assert h_proof == SparseMerkleTree.prove(tree, @data_h)

    assert valid_proof?.(@data_a, a_proof)
    assert valid_proof?.(@data_b, b_proof)
    assert valid_proof?.(@data_h, h_proof)

    refute valid_proof?.(@data_a, b_proof)
    refute valid_proof?.(@data_b, a_proof)
    refute valid_proof?.(@data_a, empty_proof)
    refute valid_proof?.(@data_c, b_proof)

    # Bad Input
    assert_raise RuntimeError, fn -> valid_proof?.(@data_a, {true, []}) end
    refute valid_proof?.(@data_a, {false, []})
    assert_raise RuntimeError, fn -> valid_proof?.(@data_a, {true, ["abcdefghijklmnopqrstuvwxyz123456"]}) end

    assert_raise RuntimeError, fn -> SparseMerkleTree.prove(tree, @data_c) end

    tree
  end
end
