defmodule Examples.ESparseMerkleTree do
  import SparseMerkleTree

  use Memoize

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
end
