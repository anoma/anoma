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
  ##                  SLICE POLYNOMIAL TERM TESTS                   ##
  ####################################################################
  # Define an expression language with arithmetic and boolean expressions
  # ArithExpr (Type 0):
  #   - Num (Constructor 0, arity 0)
  #   - Add (Constructor 1, arity 2, params: [ArithExpr, ArithExpr])
  #   - IfThenElse (Constructor 2, arity 3, params: [BoolExpr, ArithExpr, ArithExpr])
  # BoolExpr (Type 1):
  #   - Bool (Constructor 0, arity 0)
  #   - Less (Constructor 1, arity 2, params: [ArithExpr, ArithExpr])
  #   - And (Constructor 2, arity 2, params: [BoolExpr, BoolExpr])

  # Helper for testing the FinSlicePolyF module
  # We're going to define a simple type system with two types:
  #
  # Type 0: ArithExpr with constructors:
  #   - Zero (index 0, no parameters) - representing the number 0
  #   - Successor (index 1, one ArithExpr parameter) - representing n+1
  #   - Add (index 2, two ArithExpr parameters) - representing addition
  #   - IfThenElse (index 3, one BoolExpr and two ArithExpr parameters)
  #
  # Type 1: BoolExpr with constructors:
  #   - True (index 0, no parameters) - representing boolean true
  #   - False (index 1, no parameters) - representing boolean false
  #   - Less (index 2, two ArithExpr parameters) - representing comparison
  #   - And (index 3, two BoolExpr parameters) - representing logical AND
  defp create_expr_typespec() do
    alias NockPoly.FinSlicePolyF, as: SliceF

    # Create a simplified typespec manually rather than using the helpers
    # This gives us more control for testing purposes
    typespec = %{
      # Two types: ArithExpr (0) and BoolExpr (1)
      input_types: 2,
      # Same output types
      output_types: 2,
      # Each type has 4 constructors
      ctor_counts: [4, 4],

      # Define constructor types function to explicitly map each constructor to its parameter types
      ctor_types: fn
        # ArithExpr.Zero (constructor 0) has no parameters
        {0, 0} -> []
        # ArithExpr.Successor (constructor 1) takes one ArithExpr parameter
        {0, 1} -> [0]
        # ArithExpr.Add (constructor 2) takes two ArithExpr parameters
        {0, 2} -> [0, 0]
        # ArithExpr.IfThenElse (constructor 3) takes BoolExpr and two ArithExpr parameters
        {0, 3} -> [1, 0, 0]
        # BoolExpr.True (constructor 0) has no parameters
        {1, 0} -> []
        # BoolExpr.False (constructor 1) has no parameters
        {1, 1} -> []
        # BoolExpr.Less (constructor 2) takes two ArithExpr parameters
        {1, 2} -> [0, 0]
        # BoolExpr.And (constructor 3) takes two BoolExpr parameters
        {1, 3} -> [1, 1]
      end
    }

    typespec
  end

  # Helper to map our readable constructors to typespec indices
  defp create_expr_tspec() do
    fn
      # ArithExpr constructors
      # Zero (no parameters)
      {:zero} -> {:ok, {0, 0}}
      # Successor (one ArithExpr parameter)
      {:succ} -> {:ok, {0, 1}}
      # Add (two ArithExpr parameters)
      {:add} -> {:ok, {0, 2}}
      # IfThenElse (BoolExpr, ArithExpr, ArithExpr)
      {:if_then_else} -> {:ok, {0, 3}}
      # BoolExpr constructors
      # True (no parameters)
      {true} -> {:ok, {1, 0}}
      # False (no parameters)
      {false} -> {:ok, {1, 1}}
      # Less (two ArithExpr parameters)
      {:less} -> {:ok, {1, 2}}
      # And (two BoolExpr parameters)
      {:and} -> {:ok, {1, 3}}
      # Invalid constructor
      _ -> {:invalid_constructor}
    end
  end

  @doc """
  slice_test_typespec_validation: Tests that typespec validation works correctly.
  """
  def slice_test_typespec_validation() do
    alias NockPoly.FinSlicePolyF, as: SliceF

    typespec = create_expr_typespec()
    assert SliceF.validate_typespec(typespec) == :ok

    # Test validation with invalid ctor_counts
    # Too short for our 2 types
    invalid_typespec = %{typespec | ctor_counts: [3]}

    assert SliceF.validate_typespec(invalid_typespec) ==
             {:error, :ctor_counts_length_mismatch}

    typespec
  end

  @doc """
  slice_test_simple_arith: Tests simple arithmetic expressions.
  """
  def slice_test_simple_arith() do
    alias NockPoly.FinSlicePolyF, as: SliceF

    typespec = create_expr_typespec()
    tspec = create_expr_tspec()

    # Zero (representing the number 0)
    zero_term = {{:zero}, []}
    assert {:ok, 0} = SliceF.typecheck(zero_term, typespec, tspec)

    # Successor(Zero) (representing the number 1)
    one_term = {{:succ}, [zero_term]}
    assert {:ok, 0} = SliceF.typecheck(one_term, typespec, tspec)

    # Successor(One) (representing the number 2)
    two_term = {{:succ}, [one_term]}
    assert {:ok, 0} = SliceF.typecheck(two_term, typespec, tspec)

    # Add(One, One) (representing 1+1)
    add_term = {{:add}, [one_term, one_term]}
    assert {:ok, 0} = SliceF.typecheck(add_term, typespec, tspec)

    add_term
  end

  @doc """
  slice_test_simple_bool: Tests simple boolean expressions.
  """
  def slice_test_simple_bool() do
    alias NockPoly.FinSlicePolyF, as: SliceF

    typespec = create_expr_typespec()
    tspec = create_expr_tspec()

    # True (boolean constant)
    true_term = {{true}, []}
    assert {:ok, 1} = SliceF.typecheck(true_term, typespec, tspec)

    # False (boolean constant)
    false_term = {{false}, []}
    assert {:ok, 1} = SliceF.typecheck(false_term, typespec, tspec)

    # Get number terms for comparisons
    zero_term = {{:zero}, []}
    one_term = {{:succ}, [zero_term]}

    # Less(Zero, One) (representing 0 < 1)
    less_term = {{:less}, [zero_term, one_term]}
    assert {:ok, 1} = SliceF.typecheck(less_term, typespec, tspec)

    # And(True, False) (representing true AND false)
    and_term = {{:and}, [true_term, false_term]}
    assert {:ok, 1} = SliceF.typecheck(and_term, typespec, tspec)

    and_term
  end

  @doc """
  slice_test_complex: Tests a complex expression with both arithmetic and boolean expressions.
  """
  def slice_test_complex() do
    alias NockPoly.FinSlicePolyF, as: SliceF

    typespec = create_expr_typespec()
    tspec = create_expr_tspec()

    # Create our base number terms
    zero_term = {{:zero}, []}
    one_term = {{:succ}, [zero_term]}
    two_term = {{:succ}, [one_term]}

    # IfThenElse(Less(One, Two), Zero, Add(One, Two))
    if_term =
      {{:if_then_else},
       [
         # condition: Less(One, Two)
         {{:less}, [one_term, two_term]},
         # then branch: Zero
         zero_term,
         # else branch: Add(One, Two)
         {{:add}, [one_term, two_term]}
       ]}

    assert {:ok, 0} = SliceF.typecheck(if_term, typespec, tspec)

    if_term
  end

  @doc """
  slice_test_invalid_type: Tests a term with invalid parameter type.
  """
  def slice_test_invalid_type() do
    alias NockPoly.FinSlicePolyF, as: SliceF

    typespec = create_expr_typespec()
    tspec = create_expr_tspec()

    # Create base terms
    zero_term = {{:zero}, []}
    true_term = {{true}, []}

    # Error: Add takes arithmetic expressions, not boolean expressions
    # Add(Zero, True) - second parameter has wrong type
    invalid_term = {{:add}, [zero_term, true_term]}

    {:error, errors} = SliceF.typecheck(invalid_term, typespec, tspec)
    # We expect a single parameter type error
    assert Enum.any?(errors, fn e ->
             match?({:invalid_param_type, {0, 2}, 1, 1}, e)
           end)

    invalid_term
  end

  @doc """
  slice_test_invalid_arity: Tests a term with wrong parameter count.
  """
  def slice_test_invalid_arity() do
    alias NockPoly.FinSlicePolyF, as: SliceF

    typespec = create_expr_typespec()
    tspec = create_expr_tspec()

    # Create base term
    zero_term = {{:zero}, []}

    # Error: Add should have 2 parameters but has 1
    invalid_term = {{:add}, [zero_term]}

    {:error, errors} = SliceF.typecheck(invalid_term, typespec, tspec)
    assert length(errors) == 1
    assert Enum.at(errors, 0) == {:invalid_param_count, {0, 2}, 2, 1}

    invalid_term
  end

  @doc """
  slice_test_multi_errors: Tests a term with multiple type errors.
  """
  def slice_test_multi_errors() do
    alias NockPoly.FinSlicePolyF, as: SliceF

    typespec = create_expr_typespec()
    tspec = create_expr_tspec()

    # Create base terms
    zero_term = {{:zero}, []}
    true_term = {{true}, []}

    # Error: And should take 2 BoolExpr, but has 1 BoolExpr and 1 ArithExpr
    # Also, the Add has wrong parameter count (1 instead of 2)
    invalid_term =
      {{:and},
       [
         # This is a valid BoolExpr
         true_term,
         # This is an ArithExpr with wrong param count
         {{:add}, [zero_term]}
       ]}

    {:error, errors} = SliceF.typecheck(invalid_term, typespec, tspec)

    # We expect at least one error related to parameter count
    assert Enum.any?(errors, fn e ->
             match?({:invalid_param_count, _, _, _}, e)
           end)

    # In this test we're only checking for the parameter count error.
    # Depending on the implementation, we might also see a type error, but
    # it's not required - the important thing is that we detect the term is invalid
    # and return at least one error.

    invalid_term
  end

  @doc """
  slice_test_simple_type: Tests the simple_type helper creates a correct typespec.
  """
  def slice_test_simple_type() do
    alias NockPoly.FinSlicePolyF, as: SliceF

    typespec = SliceF.simple_type([0, 2, 1])
    assert typespec.input_types == 1
    assert typespec.output_types == 1
    assert typespec.ctor_counts == [3]

    # Should create constructors with the specified arities
    assert typespec.ctor_types.({0, 0}) == []
    assert typespec.ctor_types.({0, 1}) == [0, 0]
    assert typespec.ctor_types.({0, 2}) == [0]

    typespec
  end

  @doc """
  slice_test_adapt_tspec: Tests that adapt_fin_tspec correctly converts a FinPolyF tspec.
  """
  def slice_test_adapt_tspec() do
    alias NockPoly.FinSlicePolyF, as: SliceF

    # Create a FinPolyF tspec
    fin_tspec = fn
      :a -> {:ok, 0}
      :b -> {:ok, 2}
      _ -> {:invalid_constructor}
    end

    # Map constructors to indices
    ctor_indices = %{:a => 0, :b => 1}

    # Convert to FinSlicePolyF tspec
    slice_tspec = SliceF.adapt_fin_tspec(fin_tspec, ctor_indices)

    # Test converted tspec
    assert slice_tspec.(:a) == {:ok, {0, 0}}
    assert slice_tspec.(:b) == {:ok, {0, 1}}
    assert slice_tspec.(:c) == {:invalid_constructor}

    slice_tspec
  end

  ####################################################################
  ##                   EXPRESSION EVALUATOR                         ##
  ####################################################################

  @doc """
  Evaluates an arithmetic or boolean expression to its corresponding Elixir value.

  For arithmetic expressions, it returns a non-negative integer.
  For boolean expressions, it returns a boolean value (true or false).

  Assumes the expression has already been typechecked.
  """
  def evaluate_expr(term) do
    alias NockPoly.Term, as: Term

    # Define an algebra that interprets each constructor
    algebra = fn {ctor, children} ->
      case ctor do
        # ArithExpr.Zero - represents 0
        {:zero} ->
          0

        # ArithExpr.Successor - represents n+1
        {:succ} ->
          [n] = children
          n + 1

        # ArithExpr.Add - represents addition
        {:add} ->
          [a, b] = children
          a + b

        # ArithExpr.IfThenElse - represents conditional
        {:if_then_else} ->
          [condition, then_branch, else_branch] = children
          if condition, do: then_branch, else: else_branch

        # BoolExpr.True - represents boolean true
        {true} ->
          true

        # BoolExpr.False - represents boolean false
        {false} ->
          false

        # BoolExpr.Less - represents < comparison
        {:less} ->
          [a, b] = children
          a < b

        # BoolExpr.And - represents logical AND
        {:and} ->
          [a, b] = children
          a and b
      end
    end

    # Use cata to evaluate the term with our algebra
    Term.cata(term, algebra)
  end

  @doc """
  Tests the expression evaluator with a variety of expressions.
  """
  def evaluate_expr_test() do
    # Create our base terms
    zero_term = {{:zero}, []}
    one_term = {{:succ}, [zero_term]}
    two_term = {{:succ}, [one_term]}

    # Test arithmetic expressions
    assert evaluate_expr(zero_term) == 0
    assert evaluate_expr(one_term) == 1
    assert evaluate_expr(two_term) == 2

    # Test addition
    add_term = {{:add}, [one_term, two_term]}
    assert evaluate_expr(add_term) == 3

    # Test boolean expressions
    true_term = {{true}, []}
    false_term = {{false}, []}
    assert evaluate_expr(true_term) == true
    assert evaluate_expr(false_term) == false

    # Test comparison
    less_term = {{:less}, [one_term, two_term]}
    assert evaluate_expr(less_term) == true

    not_less_term = {{:less}, [two_term, one_term]}
    assert evaluate_expr(not_less_term) == false

    # Test logical AND
    and_term = {{:and}, [true_term, false_term]}
    assert evaluate_expr(and_term) == false

    and_true_term = {{:and}, [true_term, true_term]}
    assert evaluate_expr(and_true_term) == true

    # Test complex conditional expression
    # if (1 < 2) then 0 else (1 + 2)
    if_term =
      {{:if_then_else},
       [
         # condition: Less(One, Two)
         {{:less}, [one_term, two_term]},
         # then branch: Zero
         zero_term,
         # else branch: Add(One, Two)
         {{:add}, [one_term, two_term]}
       ]}

    # Since 1 < 2 is true, this should evaluate to 0
    assert evaluate_expr(if_term) == 0

    # Now let's create an expression where the condition is false
    # if (2 < 1) then 0 else (1 + 2)
    if_false_term =
      {{:if_then_else},
       [
         # condition: Less(Two, One) - false
         {{:less}, [two_term, one_term]},
         # then branch: Zero
         zero_term,
         # else branch: Add(One, Two)
         {{:add}, [one_term, two_term]}
       ]}

    # Since 2 < 1 is false, this should evaluate to 1 + 2 = 3
    assert evaluate_expr(if_false_term) == 3

    # Return the most complex term as the result
    if_false_term
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

  # We are functions used in multiple term tests below.
  defp add1(x), do: x + 1
  defp times2(x), do: x * 2

  @doc """
  I test the application of `termfv_bimap` to a variable term.
  """
  def termfv_bimap_variable_test() do
    term = 3
    res = NockPoly.Term.termfv_bimap(&add1/1, &times2/1, term)
    assert res == 4
    res
  end

  @doc """
  I test the application of `termfv_bimap` to a constructor term.
  """
  def termfv_bimap_constructor_test() do
    term = {:a, [3, 4]}
    res = NockPoly.Term.termfv_bimap(&add1/1, &times2/1, term)
    assert res == {:a, [6, 8]}
    res
  end

  @doc """
  I test the use of `tvmap` to transform variables within a term.
  """
  def tvmap_test() do
    term = {:a, [3, {:b, [4]}]}
    res = NockPoly.Term.tvmap(&add1/1, term)
    assert res == {:a, [4, {:b, [5]}]}
    res
  end

  @doc """
  I test `tv_comult` on the output of `termfv_bimap_variable_test`.

  The free monad law states that:
      tvmap(out_tv, tv_comult(term)) == term
  """
  def tv_comult_variable_from_bimap_test() do
    term = termfv_bimap_variable_test()
    duplicated = NockPoly.Term.tv_comult(term)
    result = NockPoly.Term.tvmap(&NockPoly.Term.out_tv/1, duplicated)
    assert result == term
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
    var_term = termfv_bimap_variable_test()
    cons_term = termfv_bimap_constructor_test()
    deep_term = term_test_t6()
    term = {:a, [var_term, {:b, [cons_term, deep_term]}]}
    duplicated = NockPoly.Term.tv_comult(term)
    flattened = NockPoly.Term.tv_mult(duplicated)
    assert flattened == term
    flattened
  end

  @doc """
  I test `tv_mult` on a manually constructed nested free monad term.

  Here we construct a term of type `tv(tv(v))`:
    - The inner free monad values are created by wrapping variables with `in_tv`.
    - We then wrap a constructor term containing these inner values with `in_tv`.
  Applying `tv_mult` should flatten the structure to an ordinary free monad term.
  """
  def tv_mult_manual_test() do
    inner1 = NockPoly.Term.in_tv(3)
    inner2 = NockPoly.Term.in_tv({:b, [NockPoly.Term.in_tv(4)]})
    manual = NockPoly.Term.in_tv({:a, [inner1, inner2]})
    flattened = NockPoly.Term.tv_mult(manual)
    expected = {:a, [3, {:b, [4]}]}
    assert flattened == expected
    flattened
  end

  @doc """
  I test `tv_bind` on a variable term.
  """
  def tv_bind_variable_test() do
    term = termfv_bimap_variable_test()
    f = fn x -> NockPoly.Term.in_tv(x + 1) end
    bound = NockPoly.Term.tv_bind(f, term)
    assert bound == NockPoly.Term.out_tv(term) + 1
    bound
  end

  @doc """
  I test `tv_bind` on a hybrid term.

  We construct a term using a variable term and a constructor term:
    - `var_term` is obtained from `termfv_bimap_variable_test()` (yielding 4).
    - `cons_term` is obtained from `termfv_bimap_constructor_test()` (yielding {:a, [6, 8]}).
  Then we define `m = {:c, [var_term, cons_term]}`.
  We let `f` map any variable `x` to `in_tv({:b, [x, x + 10]})`.
  Thus:
    - For the variable branch (4), f returns `in_tv({:b, [4, 14]})`, which flattens to `{ :b, [4, 14]}`.
    - For the constructor branch, the function is applied recursively to its children. That is, for
        {:a, [6, 8]} the children become `in_tv({:b, [6, 16]})` and `in_tv({:b, [8, 18]})`
        and after flattening become `{ :b, [6, 16]}` and `{ :b, [8, 18]}`.
  The entire term then yields:
      {:c, [{:b, [4, 14]}, {:a, [{:b, [6, 16]}, {:b, [8, 18]}]}]}
  """
  def tv_bind_hybrid_test() do
    # expected 4
    var_term = termfv_bimap_variable_test()
    # expected {:a, [6, 8]}
    cons_term = termfv_bimap_constructor_test()
    m = {:c, [var_term, cons_term]}
    f = fn x -> NockPoly.Term.in_tv({:b, [x, x + 10]}) end
    bound = NockPoly.Term.tv_bind(f, m)
    expected = {:c, [{:b, [4, 14]}, {:a, [{:b, [6, 16]}, {:b, [8, 18]}]}]}
    assert bound == expected
    bound
  end

  def substitute_test_variable() do
    open_term = 7

    closed_term =
      NockTerms.substitute(open_term, fn var -> {{:atom, var + 1}, []} end)

    noun = NockTerms.to_noun(closed_term)
    ExUnit.Assertions.assert(noun == 8)
    closed_term
  end

  def substitute_test_cell() do
    open_term = {:cell, [7, {{:atom, 99}, []}]}
    {:ok, expected} = Noun.Format.parse("[70 99]")

    closed_term =
      NockTerms.substitute(open_term, fn v -> {{:atom, v * 10}, []} end)

    noun = NockTerms.to_noun(closed_term)
    ExUnit.Assertions.assert(noun == expected)
    closed_term
  end
end
