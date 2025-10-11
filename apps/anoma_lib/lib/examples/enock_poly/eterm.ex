defmodule Examples.ENockPoly.ETerm do
  use Memoize

  import ExUnit.Assertions
  alias NockPoly.Term
  import NockPoly.Term.MacroDefs
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
    res = tvc0(1)
    assert Term.depth(res) == 1
    assert Term.size(res) == 1
    res
  end

  @doc """
  t2: a closed term with two children.
  """
  def term_test_t2() do
    res = tvc(:a, [tvc0(:a), tvc0(:b)])
    assert Term.depth(res) == 2
    assert Term.size(res) == 3
    res
  end

  @doc """
  t3: a nested closed term.
  """
  def term_test_t3() do
    res = tvc(:x, [tvc(:y, [tvc0(:z)])])
    assert Term.depth(res) == 3
    assert Term.size(res) == 3
    res
  end

  @doc """
  t4: a more complex closed term.
  """
  def term_test_t4() do
    res =
      tvc(
        0,
        [
          term_test_t1(),
          tvc(2, [tvc0(3), tvc0(4)]),
          tvc0(5)
        ]
      )

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
    res =
      tvc("root", [tvc0("left"), tvc0("right")])

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
    res = tvv("x")
    assert Term.depth(res) == 0
    assert Term.size(res) == 0
    res
  end

  @doc """
  I apply `out_tv` to a variable term to obtain the underlying representation.
  """
  def term_test_v1_out_tv() do
    res = term_test_v1()
    out = Term.out_tv(res)
    # The assertion verifies that out_tv reveals the internal structure
    assert out == {:tvar, "x"}
    out
  end

  @doc """
  t5: an open term whose child is a variable.
  """
  def term_test_t5() do
    res = tvc(:a, [tvv("x")])
    assert Term.depth(res) == 1
    assert Term.size(res) == 1
    res
  end

  @doc """
  t6: an open term with multiple variables (one nested).
  """
  def term_test_t6() do
    res =
      tvc(
        :b,
        [
          tvv("x"),
          tvc(:c, [tvv("y"), tvv("z")])
        ]
      )

    assert Term.depth(res) == 2
    assert Term.size(res) == 2
    res
  end

  @doc """
  t7: an open term with no variables (a closed term viewed as open).
  """
  def term_test_t7() do
    res = tvc(:d, [tvc0(:e), tvc0(:f)])
    assert Term.depth(res) == 2
    assert Term.size(res) == 3
    res
  end

  @doc """
  I apply `out_tv` to a constructor term to obtain the underlying representation.
  """
  def term_test_t7_out_tv() do
    res = term_test_t7()
    out = Term.out_tv(res)
    # The assertion verifies that out_tv reveals the internal structure
    assert out == tfc(:d, [tvc(:e, []), tvc(:f, [])])

    out
  end

  ####################################################################
  ##                     POLY TERM TESTS                            ##
  ####################################################################
  @doc """
  poly_term_test_valid: A valid term using a string-based tspec.
  """
  def poly_term_test_valid() do
    res = tvc("one", [tvc0("zero")])
    assert FinPolyF.typecheck_v(res, {common_tspec(), common_vspec()}) == :ok
    res
  end

  @doc """
  poly_term_test_arity: Term with an arity mismatch.
  """
  def poly_term_test_arity() do
    res = tvc0("one")

    assert FinPolyF.typecheck_v(res, {common_tspec(), common_vspec()}) ==
             {:error, [{:invalid_arity, "one", 1, 0}]}

    res
  end

  @doc """
  poly_term_test_ctor: Term with an invalid constructor.
  """
  def poly_term_test_ctor() do
    res = tvc0("three")

    assert FinPolyF.typecheck_v(res, {common_tspec(), common_vspec()}) ==
             {:error, [{:invalid_constructor, "three"}]}

    res
  end

  @doc """
  poly_term_test_valid_variable: A valid variable term.
  """
  def poly_term_test_valid_variable() do
    res = tvv(2)
    assert FinPolyF.typecheck_v(res, {common_tspec(), common_vspec()}) == :ok
    res
  end

  @doc """
  poly_term_test_invalid_variable: A term with an invalid variable.
  """
  def poly_term_test_invalid_variable() do
    res = tvv(10)

    assert FinPolyF.typecheck_v(res, {common_tspec(), common_vspec()}) ==
             {:error, [{:invalid_variable, 10}]}

    res
  end

  @doc """
  poly_term_test_vspec_ok: Using vspec_ok to always succeed.
  """
  def poly_term_test_vspec_ok() do
    res = tvv(10)

    assert FinPolyF.typecheck_v(res, {common_tspec(), &FinPolyF.vspec_ok/1}) ==
             :ok

    res
  end

  @doc """
  poly_term_test_multi: A term accumulating multiple errors.
  """
  def poly_term_test_multi() do
    res = tvc("two", [tvv(10), tvc0("three")])

    expected_errors = [
      {:invalid_variable, 10},
      {:invalid_constructor, "three"}
    ]

    assert FinPolyF.typecheck_v(res, {common_tspec(), common_vspec()}) ==
             {:error, expected_errors}

    res
  end

  # Helper functions for termfv/tv tests
  defp add1(x), do: x + 1
  defp times2(x), do: x * 2

  # Helper function to transform terms
  defp term_times2(term), do: NockPoly.Term.tcmap(&times2/1, term)

  def termfv_bimap_variable_test() do
    term = tvv(3)
    # Unwrap the :in_tv tag with out_tv before applying termfv_bimap
    res = NockPoly.Term.termfv_bimap(&add1/1, &times2/1, Term.out_tv(term))
    # The result should be wrapped back in :in_tv tag for comparison
    assert res == Term.out_tv(tvv(4))
    Term.in_tv(res)
  end

  @doc """
  I test the application of `termfv_bimap` to a constructor term.
  """
  @spec termfv_bimap_constructor_test() ::
          NockPoly.Term.nat_tv(non_neg_integer())
  def termfv_bimap_constructor_test() do
    # Create a term with natural number constructor and integer-term children
    term = tvc(3, [tvc0(1), tvc0(2)])

    # Unwrap the :in_tv tag with out_tv before applying termfv_bimap
    unwrapped_term = Term.out_tv(term)

    res =
      NockPoly.Term.termfv_bimap(
        &add1/1,
        &term_times2/1,
        unwrapped_term
      )

    # Expect the internal nodes to have been transformed with times2
    expected =
      Term.out_tv(tvc(3, [tvc0(2), tvc0(4)]))

    assert res == expected

    # Wrap the result back in :in_tv
    Term.in_tv(res)
  end

  @doc """
  I test the use of `tvmap` to transform variables within a term.
  """
  def tvmap_test() do
    term =
      tvc(:a, [tvv(3), tvc(:b, [tvv(4)])])

    res = NockPoly.Term.tvmap(&add1/1, term)

    assert res ==
             tvc(:a, [
               tvv(4),
               tvc(:b, [tvv(5)])
             ])

    res
  end

  @doc """
  I test `tv_comult` on the output of `termfv_bimap_variable_test`.

  The free monad law states that:
      tvmap(out_tv, tv_comult(term)) == term
  """
  def tv_comult_variable_from_bimap_test() do
    # Variable term with value 4
    term = termfv_bimap_variable_test()

    # Apply tv_comult to get a nested term structure
    duplicated = NockPoly.Term.tv_comult(term)

    # For a variable term, comult produces a variable term containing the original term
    # The term equality assertion checks that duplicated is properly structured
    assert duplicated == tvv(term)

    duplicated
  end

  @doc """
  I test `tv_comult` on the output of `termfv_bimap_constructor_test`.

  The free monad law states that:
      tvmap(out_tv, tv_comult(term)) == term
  """
  def tv_comult_constructor_from_bimap_test() do
    term = termfv_bimap_constructor_test()
    duplicated = NockPoly.Term.tv_comult(term)
    result = NockPoly.Term.tvmap(&NockPoly.Term.out_tv/1, duplicated)
    assert result == term
    duplicated
  end

  @doc """
  I test `tv_mult` on the output of `tv_comult` for a variable term.

  The free monad law for join is that flattening a duplicated term (using `tv_mult`)
  returns the original term.
  """
  def tv_mult_variable_from_comult_test() do
    term = termfv_bimap_variable_test()
    duplicated = NockPoly.Term.tv_comult(term)
    flattened = NockPoly.Term.tv_mult(duplicated)
    assert flattened == term
    flattened
  end

  @doc """
  I test `tv_mult` on a larger term using the results of some previous examples.
  """
  def tv_mult_hybrid_test() do
    alias NockPoly.Term, as: T
    var_term = termfv_bimap_variable_test()
    cons_term = termfv_bimap_constructor_test()
    deep_term = term_test_t6()
    term = T.com_tv(:a, [var_term, T.com_tv(:b, [cons_term, deep_term])])
    duplicated = NockPoly.Term.tv_comult(term)
    flattened = NockPoly.Term.tv_mult(duplicated)
    assert flattened == term
    flattened
  end

  @doc """
  I test `tv_bind` on a variable term.
  """
  def tv_bind_variable_test() do
    # Variable term with value 4
    term = termfv_bimap_variable_test()

    # Define a binding function that increments the variable
    # Uses the utility function to create a variable term
    f = fn x -> tvv(x + 1) end

    # Apply bind
    bound = NockPoly.Term.tv_bind(f, term)

    # Verify that the binding and transformation worked
    # We know from termfv_bimap_variable_test that the original value is 4
    # So after incrementing, it should match a variable term with value 5
    assert bound == tvv(5)
    bound
  end

  @doc """
  I test `tv_bind` on a hybrid term.

  We construct a term using a variable term and a constructor term:
    - `var_term` is obtained from `termfv_bimap_variable_test()` (yielding a variable term with value 4).
    - `cons_term` is obtained from `termfv_bimap_constructor_test()` (yielding a constructor term of type :a with children [6, 8]).
  Then we define `m = T.com_tv(:c, [var_term, cons_term])`.
  We let `f` map any variable `x` to `T.com_tv(:b, [T.var_tv(x), T.var_tv(x + 10)])`.
  Thus:
    - For the variable branch (4), f returns a term with a constructor :b containing two variables with values 4 and 14.
    - For the constructor branch, the function is applied recursively to its children, transforming them accordingly.
  """
  def tv_bind_hybrid_test() do
    var_term = termfv_bimap_variable_test()
    cons_term = termfv_bimap_constructor_test()
    m = tvc(:c, [var_term, cons_term])

    f = fn x ->
      tvc(:b, [tvv(x), tvv(x + 10)])
    end

    bound = NockPoly.Term.tv_bind(f, m)

    expected =
      tvc(
        :c,
        [
          tvc(:b, [tvv(4), tvv(14)]),
          cons_term
        ]
      )

    assert bound == expected
    bound
  end

  def substitute_test_variable() do
    open_term = tvv(7)

    closed_term =
      NockTerms.substitute(open_term, fn var ->
        tvc0({:atom, var + 1})
      end)

    noun = NockTerms.to_noun(closed_term)
    ExUnit.Assertions.assert(noun == 8)
    closed_term
  end

  def substitute_test_cell() do
    open_term =
      tvc(:cell, [tvv(7), tvc0({:atom, 99})])

    {:ok, expected} = Noun.Format.parse("[70 99]")

    closed_term =
      NockTerms.substitute(open_term, fn v ->
        tvc0({:atom, v * 10})
      end)

    noun = NockTerms.to_noun(closed_term)
    ExUnit.Assertions.assert(noun == expected)
    closed_term
  end
end
