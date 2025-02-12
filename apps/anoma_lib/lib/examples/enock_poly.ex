defmodule Examples.ENockPoly do
  use Memoize

  require ExUnit.Assertions
  import ExUnit.Assertions
  import NockPoly
  alias NockPoly.Term, as: Term
  alias NockPoly.FinPolyF
  alias NockPoly.NockTerms
  alias Noun

  use TypedStruct

  # Common tspec (constructor-check function) for poly_term tests.
  defp common_tspec() do
    fn
      "zero" -> {:ok, 0}
      "one" -> {:ok, 1}
      "two" -> {:ok, 2}
      _ -> {:invalid_constructor}
    end
  end

  # Common vspec (variable-check function) for poly_term tests.
  defp common_vspec() do
    fn
      v when is_integer(v) and v >= 0 and v <= 4 -> :ok
      _ -> {:invalid_variable}
    end
  end

  ####################################################################
  ##                        TERM TESTS                              ##
  ####################################################################
  @doc """
  t1: a closed term with no children.
  """
  def term_test_t1() do
    res = {1, []}
    assert Term.depth(res) == 1
    assert Term.size(res) == 1
    res
  end

  @doc """
  t2: a closed term with two children.
  """
  def term_test_t2() do
    res = {:a, [{:a, []}, {:b, []}]}
    assert Term.depth(res) == 2
    assert Term.size(res) == 3
    res
  end

  @doc """
  t3: a nested closed term.
  """
  def term_test_t3() do
    res = {:x, [{:y, [{:z, []}]}]}
    assert Term.depth(res) == 3
    assert Term.size(res) == 3
    res
  end

  @doc """
  t4: a more complex closed term.
  """
  def term_test_t4() do
    res = Term.in_tv({0, [term_test_t1(), {2, [{3, []}, {4, []}]}, {5, []}]})
    assert Term.depth(res) == 3
    assert Term.size(res) == 6
    res
  end

  @doc """
  t8: a term tested with a custom cata algebra.
  An example of a direct call to `Term.cata/2`, since `depth` and
  `size` are defined in terms of `eval` and thus do not directly
  test the `cata` wrapper.  This also serves as an example of
  providing a custom algebra to `cata` (the algebra parameter to
  `eval` has the same signature, so it is also an example of that).
  In this example we use a string constructor type and concatenate
  all the constructor names.
  """
  def term_test_t8() do
    res = {"root", [{"left", []}, {"right", []}]}

    algebra = fn {ctor, children} ->
      to_string(ctor) <> Enum.join(children, "")
    end

    assert Term.cata(res, algebra) == "rootleftright"
    res
  end

  @doc """
  v1: a single variable (an open term), which has depth and size 0.
  """
  def term_test_v1() do
    res = "x"
    assert Term.depth(res) == 0
    assert Term.size(res) == 0
    res
  end

  @doc """
  I apply `out_tv` to a variable term (the way that we define structures in
  Elixir, we expect `out_tv` simply to be the identity).
  """
  def term_test_v1_out_tv() do
    res = term_test_v1()
    out = Term.out_tv(res)
    assert out == res
    out
  end

  @doc """
  t5: an open term whose child is a variable.
  """
  def term_test_t5() do
    res = {:a, ["x"]}
    assert Term.depth(res) == 1
    assert Term.size(res) == 1
    res
  end

  @doc """
  t6: an open term with multiple variables (one nested).
  """
  def term_test_t6() do
    res = {:b, ["x", {:c, ["y", "z"]}]}
    assert Term.depth(res) == 2
    assert Term.size(res) == 2
    res
  end

  @doc """
  t7: an open term with no variables (a closed term viewed as open).
  """
  def term_test_t7() do
    res = {:d, [{:e, []}, {:f, []}]}
    assert Term.depth(res) == 2
    assert Term.size(res) == 3
    res
  end

  @doc """
  I apply `out_tv` to a constructor term (the way that we define structures in
  Elixir, we expect `out_tv` simply to be the identity).
  """
  def term_test_t7_out_tv() do
    res = term_test_t7()
    out = Term.out_tv(res)
    assert out == res
    out
  end

  ####################################################################
  ##                     POLY TERM TESTS                            ##
  ####################################################################
  @doc """
  poly_term_test_valid: A valid term using a string-based tspec.
  """
  def poly_term_test_valid() do
    res = {"one", [{"zero", []}]}
    assert FinPolyF.typecheck_v(res, {common_tspec(), common_vspec()}) == :ok
    res
  end

  @doc """
  poly_term_test_arity: Term with an arity mismatch.
  """
  def poly_term_test_arity() do
    res = {"one", []}

    assert FinPolyF.typecheck_v(res, {common_tspec(), common_vspec()}) ==
             {:error, [{:invalid_arity, "one", 1, 0}]}

    res
  end

  @doc """
  poly_term_test_ctor: Term with an invalid constructor.
  """
  def poly_term_test_ctor() do
    res = {"three", []}

    assert FinPolyF.typecheck_v(res, {common_tspec(), common_vspec()}) ==
             {:error, [{:invalid_constructor, "three"}]}

    res
  end

  @doc """
  poly_term_test_valid_variable: A valid variable term.
  """
  def poly_term_test_valid_variable() do
    res = 2
    assert FinPolyF.typecheck_v(res, {common_tspec(), common_vspec()}) == :ok
    res
  end

  @doc """
  poly_term_test_invalid_variable: A term with an invalid variable.
  """
  def poly_term_test_invalid_variable() do
    res = 10

    assert FinPolyF.typecheck_v(res, {common_tspec(), common_vspec()}) ==
             {:error, [{:invalid_variable, 10}]}

    res
  end

  @doc """
  poly_term_test_vspec_ok: Using vspec_ok to always succeed.
  """
  def poly_term_test_vspec_ok() do
    res = 10

    assert FinPolyF.typecheck_v(res, {common_tspec(), &FinPolyF.vspec_ok/1}) ==
             :ok

    res
  end

  @doc """
  poly_term_test_multi: A term accumulating multiple errors.
  """
  def poly_term_test_multi() do
    res = {"two", [10, {"three", []}]}

    expected_errors = [
      {:invalid_variable, 10},
      {:invalid_constructor, "three"}
    ]

    assert FinPolyF.typecheck_v(res, {common_tspec(), common_vspec()}) ==
             {:error, expected_errors}

    res
  end

  ####################################################################
  ##                    NOCK TERM TESTS                             ##
  ####################################################################
  #  Tests for NockTerms conversion and typecheck invariants using
  # nouns (Nock terms) lifted from Nock examples.
  #
  #  In these tests the nouns are created by parsing string representations
  #  (as in enock.ex and nock.ex). For each noun we:
  #    - Convert it to a nock_poly_term (via `NockTerms.from_noun/1`),
  #    - Ensure it passes typecheck,
  #    - Convert it back to a Noun.t() with `NockTerms.to_noun/1` and verify round‑trip invariance.
  #
  #  We also verify that an invalid term (manually constructed) raises an error.

  @doc """
  nock_term_test_one_two: Tests conversion round-trip using a Nock term
  lifted from the one_two examples.
  """
  def nock_term_test_one_two() do
    {:ok, noun_one_two} = Noun.Format.parse("[1 2]")
    res = NockTerms.from_noun(noun_one_two)
    assert NockTerms.typecheck(res) == :ok
    rt = NockTerms.to_noun(res)
    assert rt == noun_one_two
    assert NockTerms.from_noun(rt) == res
    res
  end

  @doc """
  nock_term_test_indexed: Tests conversion round-trip using a Nock term
  lifted from the indexed_noun examples.
  """
  def nock_term_test_indexed() do
    {:ok, noun_indexed} = Noun.Format.parse("[[4 5] [12 13] 7]")
    res = NockTerms.from_noun(noun_indexed)
    assert NockTerms.typecheck(res) == :ok
    rt = NockTerms.to_noun(res)
    assert rt == noun_indexed
    assert NockTerms.from_noun(rt) == res
    res
  end

  @doc """
  nock_term_test_counter_arm: Tests conversion round-trip using a Nock term
  lifted from the counter_arm examples.
  """
  def nock_term_test_counter_arm() do
    counter_arm = """
    [ 6
      [5 [1 1] 8 [9 1.406 0 1.023] 9 2 10 [6 0 118] 0 2]
      [6 [5 [1 1] 8 [9 1.406 0 1.023] 9 2 10 [6 0 238] 0 2] [6 [5 [1 1] 8 [9 1.406 0 1.023] 9 2 10 [6 0 958] 0 2] [6 [5 [1 0] 0 446] [0 0] 6 [0 3.570] [1 0] 1 1] 1 1] 1 1]
      1
      1
    ]
    """

    noun_counter = Noun.Format.parse_always(counter_arm)
    res = NockTerms.from_noun(noun_counter)
    assert NockTerms.typecheck(res) == :ok
    rt = NockTerms.to_noun(res)
    assert rt == noun_counter
    assert NockTerms.from_noun(rt) == res
    res
  end

  @doc """
  nock_term_test_invalid: Checks that an invalid term raises an error.
  (This term is invalid because a cell should have two children, and
  this alleged cell has none.)
  """
  def nock_term_test_invalid() do
    res = {:cell, []}

    assert_raise CaseClauseError, fn ->
      NockTerms.to_noun(res)
    end

    res
  end
end
