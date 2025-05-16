defmodule Examples.ENockPoly do
  use Memoize

  require ExUnit.Assertions
  import ExUnit.Assertions
  import NockPoly
  alias NockPoly.Term, as: Term
  alias NockPoly.FinPolyF
  alias NockPoly.FinSlicePolyF, as: SliceF
  alias NockPoly.FinIndIndPolyF, as: IndIndF
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
    res = Term.com_tv(1, [])
    assert Term.depth(res) == 1
    assert Term.size(res) == 1
    res
  end

  @doc """
  t2: a closed term with two children.
  """
  def term_test_t2() do
    res = Term.com_tv(:a, [Term.com_tv(:a, []), Term.com_tv(:b, [])])
    assert Term.depth(res) == 2
    assert Term.size(res) == 3
    res
  end

  @doc """
  t3: a nested closed term.
  """
  def term_test_t3() do
    res = Term.com_tv(:x, [Term.com_tv(:y, [Term.com_tv(:z, [])])])
    assert Term.depth(res) == 3
    assert Term.size(res) == 3
    res
  end

  @doc """
  t4: a more complex closed term.
  """
  def term_test_t4() do
    res =
      Term.com_tv(
        0,
        [
          term_test_t1(),
          Term.com_tv(2, [Term.com_tv(3, []), Term.com_tv(4, [])]),
          Term.com_tv(5, [])
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
      Term.com_tv("root", [Term.com_tv("left", []), Term.com_tv("right", [])])

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
    res = Term.var_tv("x")
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
    res = Term.com_tv(:a, [Term.var_tv("x")])
    assert Term.depth(res) == 1
    assert Term.size(res) == 1
    res
  end

  @doc """
  t6: an open term with multiple variables (one nested).
  """
  def term_test_t6() do
    res =
      Term.com_tv(
        :b,
        [
          Term.var_tv("x"),
          Term.com_tv(:c, [Term.var_tv("y"), Term.var_tv("z")])
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
    res = Term.com_tv(:d, [Term.com_tv(:e, []), Term.com_tv(:f, [])])
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
    assert out == {:tcom, {:d, [Term.com_tv(:e, []), Term.com_tv(:f, [])]}}
    out
  end

  ####################################################################
  ##                     POLY TERM TESTS                            ##
  ####################################################################
  @doc """
  poly_term_test_valid: A valid term using a string-based tspec.
  """
  def poly_term_test_valid() do
    res = Term.com_tv("one", [Term.com_tv("zero", [])])
    assert FinPolyF.typecheck_v(res, {common_tspec(), common_vspec()}) == :ok
    res
  end

  @doc """
  poly_term_test_arity: Term with an arity mismatch.
  """
  def poly_term_test_arity() do
    res = Term.com_tv("one", [])

    assert FinPolyF.typecheck_v(res, {common_tspec(), common_vspec()}) ==
             {:error, [{:invalid_arity, "one", 1, 0}]}

    res
  end

  @doc """
  poly_term_test_ctor: Term with an invalid constructor.
  """
  def poly_term_test_ctor() do
    res = Term.com_tv("three", [])

    assert FinPolyF.typecheck_v(res, {common_tspec(), common_vspec()}) ==
             {:error, [{:invalid_constructor, "three"}]}

    res
  end

  @doc """
  poly_term_test_valid_variable: A valid variable term.
  """
  def poly_term_test_valid_variable() do
    res = Term.var_tv(2)
    assert FinPolyF.typecheck_v(res, {common_tspec(), common_vspec()}) == :ok
    res
  end

  @doc """
  poly_term_test_invalid_variable: A term with an invalid variable.
  """
  def poly_term_test_invalid_variable() do
    res = Term.var_tv(10)

    assert FinPolyF.typecheck_v(res, {common_tspec(), common_vspec()}) ==
             {:error, [{:invalid_variable, 10}]}

    res
  end

  @doc """
  poly_term_test_vspec_ok: Using vspec_ok to always succeed.
  """
  def poly_term_test_vspec_ok() do
    res = Term.var_tv(10)

    assert FinPolyF.typecheck_v(res, {common_tspec(), &FinPolyF.vspec_ok/1}) ==
             :ok

    res
  end

  @doc """
  poly_term_test_multi: A term accumulating multiple errors.
  """
  def poly_term_test_multi() do
    res = Term.com_tv("two", [Term.var_tv(10), Term.com_tv("three", [])])

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
    end
  end

  @doc """
  slice_test_typespec_validation: Tests that typespec validation works correctly.
  """
  def slice_test_typespec_validation() do
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
    alias NockPoly.Term, as: T
    typespec = create_expr_typespec()
    tspec = create_expr_tspec()

    # Zero (representing the number 0)
    zero_term = T.com_tv({:zero}, [])
    assert {:ok, 0} = SliceF.typecheck(zero_term, typespec, tspec)

    # Successor(Zero) (representing the number 1)
    one_term = T.com_tv({:succ}, [zero_term])
    assert {:ok, 0} = SliceF.typecheck(one_term, typespec, tspec)

    # Successor(One) (representing the number 2)
    two_term = T.com_tv({:succ}, [one_term])
    assert {:ok, 0} = SliceF.typecheck(two_term, typespec, tspec)

    # Add(One, One) (representing 1+1)
    add_term = T.com_tv({:add}, [one_term, one_term])
    assert {:ok, 0} = SliceF.typecheck(add_term, typespec, tspec)

    add_term
  end

  @doc """
  slice_test_simple_bool: Tests simple boolean expressions.
  """
  def slice_test_simple_bool() do
    alias NockPoly.Term, as: T
    typespec = create_expr_typespec()
    tspec = create_expr_tspec()

    # True (boolean constant)
    true_term = T.com_tv({true}, [])
    assert {:ok, 1} = SliceF.typecheck(true_term, typespec, tspec)

    # False (boolean constant)
    false_term = T.com_tv({false}, [])
    assert {:ok, 1} = SliceF.typecheck(false_term, typespec, tspec)

    # Get number terms for comparisons
    zero_term = T.com_tv({:zero}, [])
    one_term = T.com_tv({:succ}, [zero_term])

    # Less(Zero, One) (representing 0 < 1)
    less_term = T.com_tv({:less}, [zero_term, one_term])
    assert {:ok, 1} = SliceF.typecheck(less_term, typespec, tspec)

    # And(True, False) (representing true AND false)
    and_term = T.com_tv({:and}, [true_term, false_term])
    assert {:ok, 1} = SliceF.typecheck(and_term, typespec, tspec)

    and_term
  end

  @doc """
  slice_test_complex: Tests a complex expression with both arithmetic and boolean expressions.
  """
  def slice_test_complex() do
    alias NockPoly.Term, as: T
    typespec = create_expr_typespec()
    tspec = create_expr_tspec()

    # Create our base number terms
    zero_term = T.com_tv({:zero}, [])
    one_term = T.com_tv({:succ}, [zero_term])
    two_term = T.com_tv({:succ}, [one_term])

    # IfThenElse(Less(One, Two), Zero, Add(One, Two))
    if_term =
      T.com_tv({:if_then_else}, [
        # condition: Less(One, Two)
        T.com_tv({:less}, [one_term, two_term]),
        # then branch: Zero
        zero_term,
        # else branch: Add(One, Two)
        T.com_tv({:add}, [one_term, two_term])
      ])

    assert {:ok, 0} = SliceF.typecheck(if_term, typespec, tspec)

    if_term
  end

  @doc """
  slice_test_invalid_type: Tests a term with invalid parameter type.
  """
  def slice_test_invalid_type() do
    alias NockPoly.Term, as: T
    typespec = create_expr_typespec()
    tspec = create_expr_tspec()

    # Create base terms
    zero_term = T.com_tv({:zero}, [])
    true_term = T.com_tv({true}, [])

    # Error: Add takes arithmetic expressions, not boolean expressions
    # Add(Zero, True) - second parameter has wrong type
    invalid_term = T.com_tv({:add}, [zero_term, true_term])

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
    alias NockPoly.Term, as: T
    typespec = create_expr_typespec()
    tspec = create_expr_tspec()

    # Create base term
    zero_term = T.com_tv({:zero}, [])

    # Error: Add should have 2 parameters but has 1
    invalid_term = T.com_tv({:add}, [zero_term])

    {:error, errors} = SliceF.typecheck(invalid_term, typespec, tspec)
    assert length(errors) == 1
    assert Enum.at(errors, 0) == {:invalid_param_count, {0, 2}, 2, 1}

    invalid_term
  end

  @doc """
  slice_test_multi_errors: Tests a term with multiple type errors.
  """
  def slice_test_multi_errors() do
    alias NockPoly.Term, as: T
    typespec = create_expr_typespec()
    tspec = create_expr_tspec()

    # Create base terms
    zero_term = T.com_tv({:zero}, [])
    true_term = T.com_tv({true}, [])

    # Error: And should take 2 BoolExpr, but has 1 BoolExpr and 1 ArithExpr
    # Also, the Add has wrong parameter count (1 instead of 2)
    invalid_term =
      T.com_tv({:and}, [
        # This is a valid BoolExpr
        true_term,
        # This is an ArithExpr with wrong param count
        T.com_tv({:add}, [zero_term])
      ])

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
    alias NockPoly.Term, as: T
    zero_term = T.com_tv({:zero}, [])
    one_term = T.com_tv({:succ}, [zero_term])
    two_term = T.com_tv({:succ}, [one_term])

    # Test arithmetic expressions
    assert evaluate_expr(zero_term) == 0
    assert evaluate_expr(one_term) == 1
    assert evaluate_expr(two_term) == 2

    # Test addition
    add_term = T.com_tv({:add}, [one_term, two_term])
    assert evaluate_expr(add_term) == 3

    # Test boolean expressions
    true_term = T.com_tv({true}, [])
    false_term = T.com_tv({false}, [])
    assert evaluate_expr(true_term) == true
    assert evaluate_expr(false_term) == false

    # Test comparison
    less_term = T.com_tv({:less}, [one_term, two_term])
    assert evaluate_expr(less_term) == true

    not_less_term = T.com_tv({:less}, [two_term, one_term])
    assert evaluate_expr(not_less_term) == false

    # Test logical AND
    and_term = T.com_tv({:and}, [true_term, false_term])
    assert evaluate_expr(and_term) == false

    and_true_term = T.com_tv({:and}, [true_term, true_term])
    assert evaluate_expr(and_true_term) == true

    # Test complex conditional expression
    # if (1 < 2) then 0 else (1 + 2)
    if_term =
      T.com_tv({:if_then_else}, [
        # condition: Less(One, Two)
        T.com_tv({:less}, [one_term, two_term]),
        # then branch: Zero
        zero_term,
        # else branch: Add(One, Two)
        T.com_tv({:add}, [one_term, two_term])
      ])

    # Since 1 < 2 is true, this should evaluate to 0
    assert evaluate_expr(if_term) == 0

    # Now let's create an expression where the condition is false
    # if (2 < 1) then 0 else (1 + 2)
    if_false_term =
      T.com_tv({:if_then_else}, [
        # condition: Less(Two, One) - false
        T.com_tv({:less}, [two_term, one_term]),
        # then branch: Zero
        zero_term,
        # else branch: Add(One, Two)
        T.com_tv({:add}, [one_term, two_term])
      ])

    # Since 2 < 1 is false, this should evaluate to 1 + 2 = 3
    assert evaluate_expr(if_false_term) == 3

    # Return the most complex term as the result
    if_false_term
  end

  @doc """
  slice_test_simple_type: Tests the simple_type helper creates a correct typespec.
  """
  def slice_test_simple_type() do
    # Create a simple type with three constructors of arities 0, 2, and 1
    # The array passed to simple_type defines the arities of each constructor
    typespec = SliceF.simple_type([0, 2, 1])

    # Check basic properties
    assert typespec.input_types == 1
    assert typespec.output_types == 1
    assert typespec.ctor_counts == [3]

    # Verify constructor arities match what we specified
    # Constructor 0 should have 0 parameters (empty array)
    constructor0_params = typespec.ctor_types.({0, 0})
    assert constructor0_params == []

    # Constructor 1 should have 2 parameters
    constructor1_params = typespec.ctor_types.({0, 1})
    assert length(constructor1_params) == 2
    assert Enum.all?(constructor1_params, &(&1 == 0))

    # Constructor 2 should have 1 parameter
    constructor2_params = typespec.ctor_types.({0, 2})
    assert length(constructor2_params) == 1
    assert Enum.all?(constructor2_params, &(&1 == 0))

    typespec
  end

  @doc """
  slice_test_typecheck_v_with_variables: Tests typecheck_v with variables.
  """
  def slice_test_typecheck_v_with_variables() do
    # Create a simple typespec for two types
    typespec = %{
      input_types: 2,
      output_types: 2,
      ctor_counts: [1, 1],
      ctor_types: fn
        # Type 0 constructor takes a Type 1 parameter
        {0, 0} -> [1]
        # Type 1 constructor takes a Type 0 parameter
        {1, 0} -> [0]
      end
    }

    # Create a tspec function
    tspec = fn
      {:type0_ctor} -> {:ok, {0, 0}}
      {:type1_ctor} -> {:ok, {1, 0}}
      _ -> {:invalid_constructor}
    end

    # Create a vspec function that accepts variables and assigns a type
    vspec = fn
      {:var_type0} -> {:ok, 0}
      {:var_type1} -> {:ok, 1}
      _ -> {:invalid_variable}
    end

    # For the SlicePolyF.typecheck_v, variables must be pairs of {variable, expected_type}
    # Create various test terms
    alias NockPoly.Term, as: T
    # Valid term with type0_ctor and a type1 variable
    valid_term_with_var =
      T.com_tv({:type0_ctor}, [T.var_tv({{:var_type1}, 1})])

    # Valid term with type1_ctor and a type0 variable
    valid_term_type1 = T.com_tv({:type1_ctor}, [T.var_tv({{:var_type0}, 0})])

    # Invalid term with wrong variable type
    invalid_term_with_wrong_type =
      T.com_tv({:type0_ctor}, [T.var_tv({{:var_type0}, 1})])

    # Term with invalid variable
    term_with_invalid_var =
      T.com_tv({:type0_ctor}, [T.var_tv({{:unknown_var}, 1})])

    # Term with invalid constructor
    term_with_invalid_ctor =
      T.com_tv({:unknown_ctor}, [T.var_tv({{:var_type1}, 1})])

    # Tests
    # Valid terms should typecheck correctly
    assert {:ok, 0} =
             SliceF.typecheck_v(valid_term_with_var, {typespec, tspec, vspec})

    assert {:ok, 1} =
             SliceF.typecheck_v(valid_term_type1, {typespec, tspec, vspec})

    # Test a variable with wrong type
    {:error, errors1} =
      SliceF.typecheck_v(
        invalid_term_with_wrong_type,
        {typespec, tspec, vspec}
      )

    assert Enum.any?(errors1, fn
             {:invalid_variable_type, {:var_type0}, 1, 0} -> true
           end)

    # Test an invalid variable (one that vspec doesn't recognize)
    {:error, errors2} =
      SliceF.typecheck_v(term_with_invalid_var, {typespec, tspec, vspec})

    assert Enum.any?(errors2, fn
             {:invalid_variable_type, {:unknown_var}, 1, nil} -> true
           end)

    # Test an invalid constructor
    {:error, errors3} =
      SliceF.typecheck_v(term_with_invalid_ctor, {typespec, tspec, vspec})

    assert Enum.any?(errors3, fn
             {:invalid_constructor, {:unknown_ctor}} -> true
           end)

    valid_term_with_var
  end

  @doc """
  slice_test_adapt_tspec: Tests that adapt_fin_tspec correctly converts a FinPolyF tspec.
  """
  def slice_test_adapt_tspec() do
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

  @doc """
  slice_test_invalid_constructor: Tests that typecheck_v properly handles the
  invalid_constructor error from a tspec function.
  """
  def slice_test_invalid_constructor() do
    # Create a simple typespec
    typespec = %{
      input_types: 1,
      output_types: 1,
      ctor_counts: [1],
      ctor_types: fn
        # This function will be called during the test to verify it's being used
        {0, 0} -> []
      end
    }

    # Create a tspec function that:
    # 1. Returns valid constructor for :valid_ctor to test ctor_types usage
    # 2. Returns invalid_constructor for anything else
    test_tspec = fn
      {:valid_ctor} ->
        # This will trigger a call to ctor_types with {0, 0}
        {:ok, {0, 0}}

      _ ->
        {:invalid_constructor}
    end

    # Empty vspec function that should never be called for this test
    vspec = fn _ -> {:ok, 0} end

    alias NockPoly.Term, as: T
    # Create a simple term
    term = T.com_tv({:zero}, [])

    # Test that typecheck_v properly handles the invalid_constructor error
    {:error, errors} =
      SliceF.typecheck_v(term, {typespec, test_tspec, vspec})

    assert Enum.any?(errors, fn error ->
             match?({:invalid_constructor, _}, error)
           end)

    # Now also test a path that uses ctor_types
    # Create a valid constructor term to trigger ctor_types
    valid_term = T.com_tv({:valid_ctor}, [])

    # This will pass the tspec check and then use ctor_types to verify parameters
    assert {:ok, 0} =
             SliceF.typecheck_v(valid_term, {typespec, test_tspec, vspec})

    term
  end

  @doc """
  slice_test_constant_vspec: Tests that constant_vspec creates a vspec function
  that always returns the specified type index.
  """
  def slice_test_constant_vspec() do
    # Create a constant vspec for type 3
    vspec = SliceF.constant_vspec(3)

    # Should work for any variable
    assert vspec.(:any_var) == {:ok, 3}
    assert vspec.(123) == {:ok, 3}
    assert vspec.("string") == {:ok, 3}

    vspec
  end

  @doc """
  slice_test_create_typespec_default: Tests that create_typespec correctly creates
  a typespec with default parameter types.
  """
  def slice_test_create_typespec_default() do
    # Create a typespec with:
    # - Two types (Type 0 and Type 1)
    # - Type 0 has three constructors with arities 0, 1, 2
    # - Type 1 has two constructors with arities 1, 3
    type_defs = [
      # Type 0 constructors with arities 0, 1, 2
      [0, 1, 2],
      # Type 1 constructors with arities 1, 3
      [1, 3]
    ]

    # Create param_types function that maps all parameters to type 0
    param_types = fn {_type_idx, _ctor_idx, _param_idx} -> 0 end

    # Create the typespec with explicitly defined parameter types
    typespec = SliceF.create_typespec(type_defs, param_types)

    # Verify basic properties
    # Two types in the system
    assert typespec.input_types == 2
    # Two types produced
    assert typespec.output_types == 2
    # 3 constructors for type 0, 2 for type 1
    assert typespec.ctor_counts == [3, 2]

    # Check Type 0's constructors
    type0_ctor0_params = typespec.ctor_types.({0, 0})
    # Verify all parameters are type 0 (if there are any)
    unless Enum.empty?(type0_ctor0_params) do
      assert Enum.all?(type0_ctor0_params, &(&1 == 0))
    end

    type0_ctor1_params = typespec.ctor_types.({0, 1})
    # Constructor 1 has 1 parameter
    assert length(type0_ctor1_params) == 1
    # Default to type 0
    assert Enum.all?(type0_ctor1_params, &(&1 == 0))

    type0_ctor2_params = typespec.ctor_types.({0, 2})
    # Constructor 2 has 2 parameters
    assert length(type0_ctor2_params) == 2
    # Default to type 0
    assert Enum.all?(type0_ctor2_params, &(&1 == 0))

    # Check Type 1's constructors
    type1_ctor0_params = typespec.ctor_types.({1, 0})
    # Constructor 0 has 1 parameter
    assert length(type1_ctor0_params) == 1
    # Default to type 0
    assert Enum.all?(type1_ctor0_params, &(&1 == 0))

    type1_ctor1_params = typespec.ctor_types.({1, 1})
    # Constructor 1 has 3 parameters
    assert length(type1_ctor1_params) == 3
    # Default to type 0
    assert Enum.all?(type1_ctor1_params, &(&1 == 0))

    typespec
  end

  @doc """
  slice_test_create_typespec_custom: Tests that create_typespec correctly creates
  a typespec with custom parameter types.
  """
  def slice_test_create_typespec_custom() do
    # Define two types with custom parameter types
    type_defs = [
      # Type 0's constructors with arities 1 and 2
      [1, 2],
      # Type 1's constructor with arity 3
      [3]
    ]

    # Define a custom parameter type mapping function
    # For this example:
    # - For type 0, constructor 0, parameter 0: use type 1
    # - For type 1, constructor 0, all parameters: use type 1
    # - Otherwise, use type 0
    param_types = fn
      {0, 0, 0} -> 1
      {1, 0, _} -> 1
      _ -> 0
    end

    typespec = SliceF.create_typespec(type_defs, param_types)

    # Verify basic properties
    assert typespec.input_types == 2
    assert typespec.output_types == 2
    assert typespec.ctor_counts == [2, 1]

    # Verify first constructor of type 0 with custom parameter type
    type0_ctor0_params = typespec.ctor_types.({0, 0})
    assert length(type0_ctor0_params) == 1
    # Parameter type should be 1
    assert Enum.at(type0_ctor0_params, 0) == 1

    # Verify second constructor of type 0 with explicitly specified parameter types
    type0_ctor1_params = typespec.ctor_types.({0, 1})
    assert length(type0_ctor1_params) == 2
    # All parameters should be type 0
    assert Enum.all?(type0_ctor1_params, &(&1 == 0))

    # Verify constructor of type 1 with all custom parameter types
    type1_ctor0_params = typespec.ctor_types.({1, 0})
    assert length(type1_ctor0_params) == 3
    # All parameters should be type 1
    assert Enum.all?(type1_ctor0_params, &(&1 == 1))

    typespec
  end

  ####################################################################
  ##             INDUCTIVE-INDUCTIVE TYPE SYSTEM TESTS              ##
  ####################################################################

  @doc """
  inductive_types_example: Tests the inductive-inductive type system
  with explicitly typed constructors.

  This example demonstrates how to represent mutually recursive types
  using our tagged constructor approach with {:base, pos} and {:dep, pos}.
  """
  def inductive_types_example() do
    # Define the base type with three constructors
    st0f_f1 = [
      # Constructor 0: no parameters
      [],
      # Constructor 1: two base type fields
      [0, 0],
      # Constructor 2: three base fields, each with one dependent field
      [1, 1, 1]
    ]

    # Define the dependent type with four constructors
    st1f_f1 = [
      # Constructor 0: no parameters
      [],
      # Constructor 1: two base fields, each with one dependent field
      [1, 1],
      # Constructor 2: three base fields, each with one dependent field
      [1, 1, 1],
      # Constructor 3: two base fields, each with one dependent field
      [1, 1]
    ]

    # Define how the dependent type relates to the base type
    stnt_pos_map = [0, 1, 2, 2]

    # Detail the field mappings between types for each constructor
    st_rep_transformations = [
      # For dependent constructor 0 (maps to base constructor 0)
      %{
        base_field_map: [],
        dep_field_maps: []
      },
      # For dependent constructor 1 (maps to base constructor 1)
      %{
        base_field_map: [0, 1],
        dep_field_maps: [[], []]
      },
      # For dependent constructor 2 (maps to base constructor 2)
      %{
        base_field_map: [0, 1, 2],
        dep_field_maps: [[0], [0], [0]]
      },
      # For dependent constructor 3 (maps to base constructor 2)
      %{
        base_field_map: [0, 0, 1],
        # Corrected the last entry to [0]
        dep_field_maps: [[0], [0], [0]]
      }
    ]

    # Assemble the complete natural transformation
    stnt = %{
      pos_map: stnt_pos_map,
      rep_transformations: st_rep_transformations
    }

    # Create the slice relating the dependent type to the base type
    stf1sl = %{
      total: st1f_f1,
      projection: stnt
    }

    # Create the complete inductive-inductive type system
    stmlf = %{
      base: st0f_f1,
      slice: stf1sl
    }

    # Validate the spec before using it
    assert :ok = IndIndF.validate_ind_ind_f(stmlf)

    # Create and verify terms of the base type
    # Using {:base, pos} constructor format to explicitly tag the type

    alias NockPoly.Term, as: T
    # Base type constructor 0: no parameters
    base0_term = T.com_tv({:base, 0}, [])
    assert {:ok, 0} = IndIndF.typecheck(base0_term, stmlf)

    # Base type constructor 1: two base type parameters
    base1_term = T.com_tv({:base, 1}, [base0_term, base0_term])
    assert {:ok, 0} = IndIndF.typecheck(base1_term, stmlf)

    # Create and verify terms of the dependent type
    # Using {:dep, pos} constructor format to explicitly tag the type

    # Dependent type constructor 0: no parameters
    dep0_term = T.com_tv({:dep, 0}, [])
    assert {:ok, 1} = IndIndF.typecheck(dep0_term, stmlf)

    # Dependent type constructor 1: two base fields and two dependent fields
    # First two fields are base type, next two are dependent type (1 for each base field)
    dep1_term =
      T.com_tv({:dep, 1}, [base0_term, base0_term, dep0_term, dep0_term])

    assert {:ok, 1} = IndIndF.typecheck(dep1_term, stmlf)

    # Create a more complex term that uses both types
    # Base constructor 2: three base fields, each with one dependent field
    complex_term =
      T.com_tv({:base, 2}, [
        base0_term,
        base0_term,
        base0_term,
        dep0_term,
        dep0_term,
        dep0_term
      ])

    assert {:ok, 0} = IndIndF.typecheck(complex_term, stmlf)

    # Test an invalid term with a non-existent constructor
    invalid_term = T.com_tv({:base, 3}, [])

    assert match?(
             {:error, {:invalid_constructor, _}},
             IndIndF.typecheck(invalid_term, stmlf)
           )

    # Test an invalid format (using plain integer instead of tagged constructor)
    invalid_format_term =
      T.com_tv(2, [
        base0_term,
        base0_term,
        base0_term,
        dep0_term,
        dep0_term,
        dep0_term
      ])

    assert match?(
             {:error, {:invalid_constructor_format, _, _}},
             IndIndF.typecheck(invalid_format_term, stmlf)
           )

    # Return the complex term demonstrating the mutual recursion
    complex_term
  end

  @doc """
  Tests validation functions for the FinIndIndPolyF module.

  This test covers various validation functions:
  - validate_fin_mapping
  - validate_representable_nt
  - validate_ind_ind_f1_nt
  - validate_ind_ind_f1_slice
  - validate_ind_ind_f
  """
  def inductive_types_validation_test() do
    # Test validate_fin_mapping
    # Valid mapping
    assert :ok = IndIndF.validate_fin_mapping([0, 1, 2], 3, 3)

    # Invalid mapping length
    assert {:error, :invalid_mapping_length} =
             IndIndF.validate_fin_mapping([0, 1], 3, 3)

    # Mapping out of range
    assert {:error, :mapping_out_of_range} =
             IndIndF.validate_fin_mapping([0, 3, 1], 3, 3)

    # Create base specs for further validation testing
    # Remember: in a natural transformation, we map from dependent (source) to base (target)
    # Source with 2 base fields, each with 1 dependent field
    source_rep = [1, 1]
    # Target with 2 base fields, no dependent fields (it's the base type)
    target_rep = [0, 0]

    # Valid representable NT
    valid_rep_nt = %{
      # Source field 0 -> Target field 0, Source field 1 -> Target field 1
      base_field_map: [0, 1],
      # Empty dep maps since target has no dependent fields
      dep_field_maps: [[], []]
    }

    assert :ok =
             IndIndF.validate_representable_nt(
               valid_rep_nt,
               source_rep,
               target_rep
             )

    # Invalid base field map length
    invalid_rep_nt1 = %{
      base_field_map: [0],
      dep_field_maps: [[0], []]
    }

    assert {:error, :invalid_mapping_length} =
             IndIndF.validate_representable_nt(
               invalid_rep_nt1,
               source_rep,
               target_rep
             )

    # Invalid dep field maps length
    invalid_rep_nt2 = %{
      base_field_map: [0, 1],
      # Only one dep map but need two
      dep_field_maps: [[]]
    }

    assert {:error, :invalid_dep_field_maps_length} =
             IndIndF.validate_representable_nt(
               invalid_rep_nt2,
               source_rep,
               target_rep
             )

    # Create specs for IndIndF1 natural transformation validation
    source_ind_f1 = [
      # Constructor 0 has 2 base fields
      [1, 0],
      # Constructor 1 has 2 base fields
      [2, 1]
    ]

    target_ind_f1 = [
      # Constructor 0 has 1 base field
      [0],
      # Constructor 1 has 2 base fields
      [1, 1]
    ]

    # Valid ind_ind_f1_nt
    valid_ind_f1_nt = %{
      # Source pos 0 -> Target pos 0, Source pos 1 -> Target pos 1
      pos_map: [0, 1],
      rep_transformations: [
        # For source pos 0 -> target pos 0
        %{
          # Target base field 0 -> Source base field 0
          base_field_map: [0],
          # No dep fields for target
          dep_field_maps: [[]]
        },
        # For source pos 1 -> target pos 1
        %{
          # Maps target fields to source fields
          base_field_map: [0, 1],
          # Dep maps for each target field
          dep_field_maps: [[0], [0]]
        }
      ]
    }

    # Test validate_ind_ind_f1_nt with valid input
    assert :ok =
             IndIndF.validate_ind_ind_f1_nt(
               valid_ind_f1_nt,
               source_ind_f1,
               target_ind_f1
             )

    # Invalid pos_map length
    invalid_ind_f1_nt1 = %{
      # Too short
      pos_map: [0],
      rep_transformations: [
        %{
          base_field_map: [0],
          dep_field_maps: [[]]
        }
      ]
    }

    assert {:error, :invalid_mapping_length} =
             IndIndF.validate_ind_ind_f1_nt(
               invalid_ind_f1_nt1,
               source_ind_f1,
               target_ind_f1
             )

    # Invalid rep_transformations length
    invalid_ind_f1_nt2 = %{
      pos_map: [0, 1],
      rep_transformations: [
        %{
          base_field_map: [0],
          dep_field_maps: [[]]
        }
        # Missing the second transformation
      ]
    }

    assert {:error, :invalid_rep_transformations_length} =
             IndIndF.validate_ind_ind_f1_nt(
               invalid_ind_f1_nt2,
               source_ind_f1,
               target_ind_f1
             )

    # Setup for slice validation
    # Two constructors with no dep fields
    base_type = [[], [0, 0]]

    # The dependent type needs consistent dep field counts
    # Two constructors with 0 and 2 base fields, and dep fields
    dep_type = [[], [1, 1]]

    valid_slice = %{
      total: dep_type,
      projection: %{
        pos_map: [0, 1],
        rep_transformations: [
          %{
            base_field_map: [],
            # First constructor has no base fields
            dep_field_maps: []
          },
          %{
            # Second constructor maps to base fields 0 and 1
            base_field_map: [0, 1],
            # One dep field for each base field
            dep_field_maps: [[], []]
          }
        ]
      }
    }

    # No need to validate the base and dependent types separately
    # as they're always well-formed by construction

    # Now test validate_ind_ind_f1_slice
    assert :ok = IndIndF.validate_ind_ind_f1_slice(valid_slice, base_type)

    # Invalid source for projection
    invalid_slice = %{
      # Only one constructor, causing mismatch with projection
      total: [[]],
      # Projection expects two constructors
      projection: valid_slice.projection
    }

    assert {:error, :invalid_mapping_length} =
             IndIndF.validate_ind_ind_f1_slice(invalid_slice, base_type)

    # Test validate_ind_ind_f
    valid_ind_ind_f = %{
      base: base_type,
      slice: valid_slice
    }

    assert :ok = IndIndF.validate_ind_ind_f(valid_ind_ind_f)

    # Return the valid inductive-inductive type system
    valid_ind_ind_f
  end

  @doc """
  Tests error cases in FinIndIndPolyF.typecheck_base_constructor and typecheck_dep_constructor.

  This test covers error handling and validation of fields.
  """
  def inductive_types_error_test() do
    # Create a minimal inductive-inductive type system
    # Two constructors: one with no fields, one with two base fields
    base_type = [[], [0, 0]]

    # Dependent type with similar structure
    # Two constructors: one with no fields, one with two base fields + deps
    dep_type = [[], [1, 1]]

    # Create the natural transformation
    projection = %{
      # Straightforward mapping
      pos_map: [0, 1],
      rep_transformations: [
        %{
          base_field_map: [],
          dep_field_maps: []
        },
        %{
          base_field_map: [0, 1],
          dep_field_maps: [[0], [0]]
        }
      ]
    }

    # Complete system
    stmlf = %{
      base: base_type,
      slice: %{
        total: dep_type,
        projection: projection
      }
    }

    alias NockPoly.Term, as: T
    # Valid terms for testing
    base0_term = T.com_tv({:base, 0}, [])
    dep0_term = T.com_tv({:dep, 0}, [])

    # Test field count errors in base constructor
    # Should have 2 fields
    invalid_field_count_base = T.com_tv({:base, 1}, [base0_term])

    assert {:error, {:invalid_field_count, _, 2, 1}} =
             IndIndF.typecheck(invalid_field_count_base, stmlf)

    # Test field count errors in dependent constructor
    # Should have 4 fields (2 base + 2 dep)
    invalid_field_count_dep = T.com_tv({:dep, 1}, [base0_term])

    assert {:error, {:invalid_field_count, _, 4, 1}} =
             IndIndF.typecheck(invalid_field_count_dep, stmlf)

    # Create a valid constructor with fields of correct type
    valid_base_term = T.com_tv({:base, 1}, [base0_term, base0_term])
    assert {:ok, 0} = IndIndF.typecheck(valid_base_term, stmlf)

    # Create a term with mixed base and dependent type terms
    # This should fail type checking because dep0_term is not allowed as a field
    # in a base type constructor
    invalid_type_term = T.com_tv({:base, 1}, [base0_term, dep0_term])
    {:error, error_info} = IndIndF.typecheck(invalid_type_term, stmlf)

    # Verify the error is of the form {:invalid_fields, _}
    assert match?({:invalid_fields, _}, error_info)

    # Test invalid term format - using a term with an invalid structure
    # but still with the proper tagged variant format
    invalid_term_format = T.in_tv({:tcom, {:unknown_format, []}})

    assert match?(
             {:error, {:invalid_constructor_format, _, _}},
             IndIndF.typecheck(invalid_term_format, stmlf)
           )

    # Test error propagation with check_fields function
    # Create an invalid field that will fail typecheck
    # Constructor index 5 doesn't exist
    invalid_field = T.com_tv({:base, 5}, [])

    # Use the invalid field in a valid constructor
    term_with_invalid_field =
      T.com_tv({:base, 1}, [invalid_field, invalid_field])

    assert {:error, {:invalid_fields, _}} =
             IndIndF.typecheck(term_with_invalid_field, stmlf)

    stmlf
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
    alias NockPoly.Term, as: T
    # Create an invalid cell (missing children) but with the proper structure
    res = T.com_tv(:cell, [])

    assert_raise CaseClauseError, fn ->
      NockTerms.to_noun(res)
    end

    res
  end

  # Functions used in termfv_bimap tests
  # These operate on raw values for the bimap tests
  defp add1(x), do: x + 1
  defp times2(x), do: x * 2

  # Functions that operate on terms using tvmap
  defp term_times2(term), do: NockPoly.Term.tcmap(&times2/1, term)

  @doc """
  I test the application of `termfv_bimap` to a variable term.
  """
  def termfv_bimap_variable_test() do
    term = Term.var_tv(3)
    # Unwrap the :in_tv tag with out_tv before applying termfv_bimap
    res = NockPoly.Term.termfv_bimap(&add1/1, &times2/1, Term.out_tv(term))
    # The result should be wrapped back in :in_tv tag for comparison
    assert res == Term.out_tv(Term.var_tv(4))
    Term.in_tv(res)
  end

  @doc """
  I test the application of `termfv_bimap` to a constructor term.
  """
  @spec termfv_bimap_constructor_test() ::
          NockPoly.Term.nat_tv(non_neg_integer())
  def termfv_bimap_constructor_test() do
    # Create a term with natural number constructor and integer-term children
    term = Term.com_tv(3, [Term.com_tv(1, []), Term.com_tv(2, [])])

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
      Term.out_tv(Term.com_tv(3, [Term.com_tv(2, []), Term.com_tv(4, [])]))

    assert res == expected

    # Wrap the result back in :in_tv
    Term.in_tv(res)
  end

  @doc """
  I test the use of `tvmap` to transform variables within a term.
  """
  def tvmap_test() do
    term =
      Term.com_tv(:a, [Term.var_tv(3), Term.com_tv(:b, [Term.var_tv(4)])])

    res = NockPoly.Term.tvmap(&add1/1, term)

    assert res ==
             Term.com_tv(:a, [
               Term.var_tv(4),
               Term.com_tv(:b, [Term.var_tv(5)])
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
    assert duplicated == Term.var_tv(term)

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
    f = fn x -> Term.var_tv(x + 1) end

    # Apply bind
    bound = NockPoly.Term.tv_bind(f, term)

    # Verify that the binding and transformation worked
    # We know from termfv_bimap_variable_test that the original value is 4
    # So after incrementing, it should match a variable term with value 5
    assert bound == Term.var_tv(5)
    bound
  end

  @doc """
  I test `tv_bind` on a hybrid term.

  We construct a term using a variable term and a constructor term:
    - `var_term` is obtained from `termfv_bimap_variable_test()` (yielding {:tvar, 4}).
    - `cons_term` is obtained from `termfv_bimap_constructor_test()` (yielding {:tcom, {:a, [6, 8]}}).
  Then we define `m = {:tcom, {:c, [var_term, cons_term]}}`.
  We let `f` map any variable `x` to `in_tv({:tcom, {:b, [{:tvar, x}, {:tvar, x + 10}]}})`.
  Thus:
    - For the variable branch (4), f returns a term that flattens to `{:tcom, {:b, [{:tvar, 4}, {:tvar, 14}]}}`.
    - For the constructor branch, the function is applied recursively to its children, transforming them accordingly.
  """
  def tv_bind_hybrid_test() do
    var_term = termfv_bimap_variable_test()
    cons_term = termfv_bimap_constructor_test()
    m = Term.com_tv(:c, [var_term, cons_term])

    f = fn x ->
      Term.com_tv(:b, [Term.var_tv(x), Term.var_tv(x + 10)])
    end

    bound = NockPoly.Term.tv_bind(f, m)

    expected =
      Term.com_tv(
        :c,
        [
          Term.com_tv(:b, [Term.var_tv(4), Term.var_tv(14)]),
          cons_term
        ]
      )

    assert bound == expected
    bound
  end

  def substitute_test_variable() do
    open_term = Term.var_tv(7)

    closed_term =
      NockTerms.substitute(open_term, fn var ->
        Term.com_tv({:atom, var + 1}, [])
      end)

    noun = NockTerms.to_noun(closed_term)
    ExUnit.Assertions.assert(noun == 8)
    closed_term
  end

  def substitute_test_cell() do
    open_term =
      Term.com_tv(:cell, [Term.var_tv(7), Term.com_tv({:atom, 99}, [])])

    {:ok, expected} = Noun.Format.parse("[70 99]")

    closed_term =
      NockTerms.substitute(open_term, fn v ->
        Term.com_tv({:atom, v * 10}, [])
      end)

    noun = NockTerms.to_noun(closed_term)
    ExUnit.Assertions.assert(noun == expected)
    closed_term
  end
end
