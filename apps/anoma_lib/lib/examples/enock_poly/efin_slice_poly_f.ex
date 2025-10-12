defmodule Examples.ENockPoly.EFinSlicePolyF do
  use Memoize

  import ExUnit.Assertions
  import NockPoly
  alias NockPoly.Term, as: Term
  import NockPoly.Term.MacroDefs
  alias NockPoly.FinSlicePolyF, as: SliceF

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
    typespec = %SliceF{
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
    invalid_typespec = %SliceF{typespec | ctor_counts: [3]}

    assert SliceF.validate_typespec(invalid_typespec) ==
             {:error, :ctor_counts_length_mismatch}

    typespec
  end

  @doc """
  slice_test_simple_arith: Tests simple arithmetic expressions.
  """
  def slice_test_simple_arith() do
    typespec = create_expr_typespec()
    tspec = create_expr_tspec()

    # Zero (representing the number 0)
    zero_term = tvc0({:zero})
    assert {:ok, 0} = SliceF.typecheck(zero_term, typespec, tspec)

    # Successor(Zero) (representing the number 1)
    one_term = tvc({:succ}, [zero_term])
    assert {:ok, 0} = SliceF.typecheck(one_term, typespec, tspec)

    # Successor(One) (representing the number 2)
    two_term = tvc({:succ}, [one_term])
    assert {:ok, 0} = SliceF.typecheck(two_term, typespec, tspec)

    # Add(One, One) (representing 1+1)
    add_term = tvc({:add}, [one_term, one_term])
    assert {:ok, 0} = SliceF.typecheck(add_term, typespec, tspec)

    add_term
  end

  @doc """
  slice_test_simple_bool: Tests simple boolean expressions.
  """
  def slice_test_simple_bool() do
    typespec = create_expr_typespec()
    tspec = create_expr_tspec()

    # True (boolean constant)
    true_term = tvc0({true})
    assert {:ok, 1} = SliceF.typecheck(true_term, typespec, tspec)

    # False (boolean constant)
    false_term = tvc0({false})
    assert {:ok, 1} = SliceF.typecheck(false_term, typespec, tspec)

    # Get number terms for comparisons
    zero_term = tvc0({:zero})
    one_term = tvc({:succ}, [zero_term])

    # Less(Zero, One) (representing 0 < 1)
    less_term = tvc({:less}, [zero_term, one_term])
    assert {:ok, 1} = SliceF.typecheck(less_term, typespec, tspec)

    # And(True, False) (representing true AND false)
    and_term = tvc({:and}, [true_term, false_term])
    assert {:ok, 1} = SliceF.typecheck(and_term, typespec, tspec)

    and_term
  end

  @doc """
  slice_test_complex: Tests a complex expression with both arithmetic and boolean expressions.
  """
  def slice_test_complex() do
    typespec = create_expr_typespec()
    tspec = create_expr_tspec()

    # Create our base number terms
    zero_term = tvc0({:zero})
    one_term = tvc({:succ}, [zero_term])
    two_term = tvc({:succ}, [one_term])

    # IfThenElse(Less(One, Two), Zero, Add(One, Two))
    if_term =
      tvc({:if_then_else}, [
        # condition: Less(One, Two)
        tvc({:less}, [one_term, two_term]),
        # then branch: Zero
        zero_term,
        # else branch: Add(One, Two)
        tvc({:add}, [one_term, two_term])
      ])

    assert {:ok, 0} = SliceF.typecheck(if_term, typespec, tspec)

    if_term
  end

  @doc """
  slice_test_invalid_type: Tests a term with invalid parameter type.
  """
  def slice_test_invalid_type() do
    typespec = create_expr_typespec()
    tspec = create_expr_tspec()

    # Create base terms
    zero_term = tvc0({:zero})
    true_term = tvc0({true})

    # Error: Add takes arithmetic expressions, not boolean expressions
    # Add(Zero, True) - second parameter has wrong type
    invalid_term = tvc({:add}, [zero_term, true_term])

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
    typespec = create_expr_typespec()
    tspec = create_expr_tspec()

    # Create base term
    zero_term = tvc0({:zero})

    # Error: Add should have 2 parameters but has 1
    invalid_term = tvc({:add}, [zero_term])

    {:error, errors} = SliceF.typecheck(invalid_term, typespec, tspec)
    assert length(errors) == 1
    assert Enum.at(errors, 0) == {:invalid_param_count, {0, 2}, 2, 1}

    invalid_term
  end

  @doc """
  slice_test_multi_errors: Tests a term with multiple type errors.
  """
  def slice_test_multi_errors() do
    typespec = create_expr_typespec()
    tspec = create_expr_tspec()

    # Create base terms
    zero_term = tvc0({:zero})
    true_term = tvc0({true})

    # Error: And should take 2 BoolExpr, but has 1 BoolExpr and 1 ArithExpr
    # Also, the Add has wrong parameter count (1 instead of 2)
    invalid_term =
      tvc({:and}, [
        # This is a valid BoolExpr
        true_term,
        # This is an ArithExpr with wrong param count
        tvc({:add}, [zero_term])
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
    zero_term = tvc0({:zero})
    one_term = tvc({:succ}, [zero_term])
    two_term = tvc({:succ}, [one_term])

    # Test arithmetic expressions
    assert evaluate_expr(zero_term) == 0
    assert evaluate_expr(one_term) == 1
    assert evaluate_expr(two_term) == 2

    # Test addition
    add_term = tvc({:add}, [one_term, two_term])
    assert evaluate_expr(add_term) == 3

    # Test boolean expressions
    true_term = tvc0({true})
    false_term = tvc0({false})
    assert evaluate_expr(true_term) == true
    assert evaluate_expr(false_term) == false

    # Test comparison
    less_term = tvc({:less}, [one_term, two_term])
    assert evaluate_expr(less_term) == true

    not_less_term = tvc({:less}, [two_term, one_term])
    assert evaluate_expr(not_less_term) == false

    # Test logical AND
    and_term = tvc({:and}, [true_term, false_term])
    assert evaluate_expr(and_term) == false

    and_true_term = tvc({:and}, [true_term, true_term])
    assert evaluate_expr(and_true_term) == true

    # Test complex conditional expression
    # if (1 < 2) then 0 else (1 + 2)
    if_term =
      tvc({:if_then_else}, [
        # condition: Less(One, Two)
        tvc({:less}, [one_term, two_term]),
        # then branch: Zero
        zero_term,
        # else branch: Add(One, Two)
        tvc({:add}, [one_term, two_term])
      ])

    # Since 1 < 2 is true, this should evaluate to 0
    assert evaluate_expr(if_term) == 0

    # Now let's create an expression where the condition is false
    # if (2 < 1) then 0 else (1 + 2)
    if_false_term =
      tvc({:if_then_else}, [
        # condition: Less(Two, One) - false
        tvc({:less}, [two_term, one_term]),
        # then branch: Zero
        zero_term,
        # else branch: Add(One, Two)
        tvc({:add}, [one_term, two_term])
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
    typespec = %SliceF{
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
      _ -> :invalid_constructor
    end

    # Create a vspec function that accepts variables and assigns a type
    vspec = fn
      {:var_type0} -> {:ok, 0}
      {:var_type1} -> {:ok, 1}
      _ -> :invalid_variable
    end

    # For the SlicePolyF.typecheck_v, variables must be pairs of {variable, expected_type}
    # Create various test terms
    # Valid term with type0_ctor and a type1 variable
    valid_term_with_var =
      tvc({:type0_ctor}, [tvv({{:var_type1}, 1})])

    # Valid term with type1_ctor and a type0 variable
    valid_term_type1 = tvc({:type1_ctor}, [tvv({{:var_type0}, 0})])

    # Invalid term with wrong variable type
    invalid_term_with_wrong_type =
      tvc({:type0_ctor}, [tvv({{:var_type0}, 1})])

    # Term with invalid variable
    term_with_invalid_var =
      tvc({:type0_ctor}, [tvv({{:unknown_var}, 1})])

    # Term with invalid constructor
    term_with_invalid_ctor =
      tvc({:unknown_ctor}, [tvv({{:var_type1}, 1})])

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
      _ -> :invalid_constructor
    end

    # Map constructors to indices
    ctor_indices = %{:a => 0, :b => 1}

    # Convert to FinSlicePolyF tspec
    slice_tspec = SliceF.adapt_fin_tspec(fin_tspec, ctor_indices)

    # Test converted tspec
    assert slice_tspec.(:a) == {:ok, {0, 0}}
    assert slice_tspec.(:b) == {:ok, {0, 1}}
    assert slice_tspec.(:c) == :invalid_constructor

    slice_tspec
  end

  @doc """
  slice_test_invalid_constructor: Tests that typecheck_v properly handles the
  invalid_constructor error from a tspec function.
  """
  def slice_test_invalid_constructor() do
    # Create a simple typespec
    typespec = %SliceF{
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
        :invalid_constructor
    end

    # Empty vspec function that should never be called for this test
    vspec = fn _ -> {:ok, 0} end

    # Create a simple term
    term = tvc0({:zero})

    # Test that typecheck_v properly handles the invalid_constructor error
    {:error, errors} =
      SliceF.typecheck_v(term, {typespec, test_tspec, vspec})

    assert Enum.any?(errors, fn error ->
             match?({:invalid_constructor, _}, error)
           end)

    # Now also test a path that uses ctor_types
    # Create a valid constructor term to trigger ctor_types
    valid_term = tvc0({:valid_ctor})

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
end
