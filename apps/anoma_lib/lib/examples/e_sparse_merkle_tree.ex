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
    {:ok, proof} = prove(tree, "abcd")
    IO.inspect(proof)
    verify(root(tree), "abcd", proof)
  end

  def prove_abc_present_in_abc_def() do
    {:ok, _} = prove(abc_def_tree(), "abc")
  end

  def prove_abc_absent_in_def() do
    {:ok, _} = prove(def_tree(), "abc")
  end

  def prove_abc_absent_in_empty() do
    {:ok, _} = prove(empty_tree(), "abc")
  end

  def dont_prove_abc_present_in_empty() do
    :error = prove(empty_tree(), "abc")
  end

  def dont_prove_abc_present_in_def() do
    :error = prove(def_tree(), "abc")
  end

  def dont_prove_abc_absent_in_abc_def() do
    :error = prove(abc_def_tree(), "abc")
  end

  def dont_prove_abc_absent_in_abc() do
    :error = prove(abc_tree(), "abc")
  end

  defmemo big_tree() do
    for n <- 1..(2 ** 16), reduce: new() do
      tree ->
        tree |> insert(Integer.to_string(n))
    end
  end
end
