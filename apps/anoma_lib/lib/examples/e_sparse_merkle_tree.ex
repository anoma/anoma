defmodule Examples.ESparseMerkleTree do
  import SparseMerkleTree

  use Memoize

  def empty_tree() do
    %{
      root:
        <<154, 89, 96, 51, 200, 43, 101, 197, 238, 240, 245, 241, 96, 185,
          201, 137, 56, 68, 118, 90, 21, 171, 104, 84, 134, 147, 28, 135, 0,
          4, 185, 16>>
    } = new()
  end

  def abc_tree() do
    %{
      root:
        <<231, 56, 170, 76, 247, 137, 193, 60, 159, 48, 99, 48, 43, 115, 2,
          188, 170, 72, 5, 29, 18, 6, 137, 43, 222, 0, 64, 229, 193, 17, 25,
          252>>
    } = empty_tree() |> insert("abc")
  end

  def def_tree() do
    %{
      root:
        <<45, 22, 3, 38, 200, 156, 104, 205, 128, 199, 139, 247, 142, 193,
          147, 174, 131, 183, 133, 165, 207, 50, 242, 177, 236, 38, 5, 154,
          216, 36, 62, 219>>
    } = empty_tree() |> insert("def")
  end

  def abc_def_tree() do
    %{
      root:
        <<205, 129, 132, 34, 91, 149, 19, 194, 217, 212, 168, 219, 219, 82,
          163, 226, 200, 187, 91, 181, 202, 214, 159, 216, 216, 79, 224, 233,
          178, 219, 210, 190>>
    } = empty_tree() |> insert("abc") |> insert("def")
  end

  def def_abc_tree() do
    %{
      root:
        <<205, 129, 132, 34, 91, 149, 19, 194, 217, 212, 168, 219, 219, 82,
          163, 226, 200, 187, 91, 181, 202, 214, 159, 216, 216, 79, 224, 233,
          178, 219, 210, 190>>
    } = empty_tree() |> insert("def") |> insert("abc")
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
    {:ok, _} = prove_present(abc_tree(), "abc")
  end

  def prove_abc_present_in_abc_def() do
    {:ok, _} = prove_present(abc_def_tree(), "abc")
  end

  def prove_abc_absent_in_def() do
    {:ok, _} = prove_absent(def_tree(), "abc")
  end

  def prove_abc_absent_in_empty() do
    {:ok, _} = prove_absent(empty_tree(), "abc")
  end

  def dont_prove_abc_present_in_empty() do
    :error = prove_present(empty_tree(), "abc")
  end

  def dont_prove_abc_present_in_def() do
    :error = prove_present(def_tree(), "abc")
  end

  def dont_prove_abc_absent_in_abc_def() do
    :error = prove_absent(abc_def_tree(), "abc")
  end

  def dont_prove_abc_absent_in_abc() do
    :error = prove_absent(abc_tree(), "abc")
  end

  defmemo big_tree() do
    for n <- 1..(2 ** 16), reduce: new() do
      tree ->
        tree |> insert(Integer.to_string(n))
    end
  end
end
