defmodule Examples.ENockPoly.EFin2ForestPolyF do
  use Memoize

  import ExUnit.Assertions
  import NockPoly
  alias NockPoly.Term
  alias NockPoly.Fin2ForestPolyF, as: Forest

  @doc """
  I demonstrate the Fin2ForestPolyF module for unified polynomial functors.

  This module unifies:
  - FinSlicePolyF: Multiple types with typed parameters
  - FinIndIndPolyF: Inductive-inductive types with dependencies
  """
  def fin2_forest_basic_test() do
    alias NockPoly.Fin2ForestPolyF, as: Forest

    # Test basic forest structure
    forest = [2, 0, 3]
    assert Forest.num_base_types(forest) == 3
    assert Forest.num_dep_types(forest, 0) == 2
    assert Forest.num_dep_types(forest, 1) == 0
    assert Forest.num_dep_types(forest, 2) == 3

    # Test forest object validation
    assert Forest.validate_forest_obj({:base, 0}, forest) == :ok
    assert Forest.validate_forest_obj({:base, 2}, forest) == :ok

    assert Forest.validate_forest_obj({:base, 3}, forest) ==
             {:error, :invalid_base_index}

    assert Forest.validate_forest_obj({:dep, 0, 1}, forest) == :ok

    assert Forest.validate_forest_obj({:dep, 0, 2}, forest) ==
             {:error, :invalid_dep_index}

    assert Forest.validate_forest_obj({:dep, 1, 0}, forest) ==
             {:error, :invalid_dep_index}

    assert Forest.validate_forest_obj({:dep, 2, 2}, forest) == :ok

    :ok
  end

  @doc """
  I test creating and validating forest specifications.
  """
  def fin2_forest_poly_spec_test() do
    alias NockPoly.Fin2ForestPolyF, as: Forest

    # Simple spec with one base type
    simple_spec = Forest.simple_forest_spec([2, 0, 1])
    assert Forest.validate_forest_spec(simple_spec) == :ok

    # Complex spec with dependent types
    # Forest [1, 0] means: base 0 has 1 dep, base 1 has 0 deps
    complex_spec =
      Forest.create_forest_spec(
        [1, 0],
        [
          # Base type 0: constructor takes two base params
          [[{:base, 0}, {:base, 1}]],
          # Base type 1: constructor takes no params
          [[]]
        ],
        [
          # Base 0 has 1 dependent
          [
            # Dependent 0's constructors
            [[{:dep, 0, 0}]]
          ],
          # Base 1 has no dependent types
          []
        ]
      )

    assert Forest.validate_forest_spec(complex_spec) == :ok

    # Test getting constructor params
    assert Forest.get_ctor_params(complex_spec, {:base, 0, 0}) ==
             {:ok, [{:base, 0}, {:base, 1}]}

    assert Forest.get_ctor_params(complex_spec, {:base, 1, 0}) ==
             {:ok, []}

    assert Forest.get_ctor_params(complex_spec, {:dep, 0, 0, 0}) ==
             {:ok, [{:dep, 0, 0}]}

    # Test getting constructor types
    assert Forest.get_ctor_type({:base, 0, 0}) == {:base, 0}
    assert Forest.get_ctor_type({:dep, 0, 0, 1}) == {:dep, 0, 0}

    :ok
  end

  @doc """
  I test typechecking terms with the forest type system.
  """
  def fin2_forest_typecheck_test() do
    alias NockPoly.Fin2ForestPolyF, as: Forest

    # Create a spec for natural numbers with zero and successor
    # zero: 0 args, succ: 1 arg
    nat_spec = Forest.simple_forest_spec([0, 1])

    # Create terms
    # zero constructor
    zero = Term.com_tv({:base, 0, 0}, [])
    # succ(zero)
    one = Term.com_tv({:base, 0, 1}, [zero])
    # succ(succ(zero))
    two = Term.com_tv({:base, 0, 1}, [one])

    # Test typechecking
    assert Forest.typecheck(zero, nat_spec) == {:ok, {:base, 0}}
    assert Forest.typecheck(one, nat_spec) == {:ok, {:base, 0}}
    assert Forest.typecheck(two, nat_spec) == {:ok, {:base, 0}}

    # Test error cases
    # zero with an argument
    bad_arity = Term.com_tv({:base, 0, 0}, [zero])

    assert {:error, [{:invalid_arity, {:base, 0, 0}, 0, 1}]} =
             Forest.typecheck(bad_arity, nat_spec)

    # Non-existent constructor
    bad_ctor = Term.com_tv({:base, 0, 2}, [])

    assert {:error, [{:invalid_constructor, {:base, 0, 2}}]} =
             Forest.typecheck(bad_ctor, nat_spec)

    :ok
  end

  @doc """
  I demonstrate using Fin2ForestPolyF to model the walking arrow category.

  The walking arrow is the simplest non-trivial forest: [1]
  - One base type with one dependent type
  """
  def fin2_forest_walking_arrow_test() do
    alias NockPoly.Fin2ForestPolyF, as: Forest

    # Walking arrow forest: one base, one dependent
    walking_arrow_spec =
      Forest.create_forest_spec(
        # One base type with one dependent
        [1],
        [
          # Base type has one constructor with no params
          [[]]
        ],
        [
          # Base 0 has 1 dependent
          [
            # Dependent 0's constructors
            [[{:base, 0}]]
          ]
        ]
      )

    assert Forest.validate_forest_spec(walking_arrow_spec) == :ok

    # Create terms
    base_obj = Term.com_tv({:base, 0, 0}, [])
    dep_obj = Term.com_tv({:dep, 0, 0, 0}, [base_obj])

    # Typecheck
    assert Forest.typecheck(base_obj, walking_arrow_spec) == {:ok, {:base, 0}}

    assert Forest.typecheck(dep_obj, walking_arrow_spec) ==
             {:ok, {:dep, 0, 0}}

    # Error: dependent constructor with wrong param type
    # Should take base, not dep
    bad_dep = Term.com_tv({:dep, 0, 0, 0}, [dep_obj])

    assert {:error, [{:invalid_param_type, {:dep, 0, 0, 0}, 0, {:dep, 0, 0}}]} =
             Forest.typecheck(bad_dep, walking_arrow_spec)

    :ok
  end

  # Helper function to create the Nat/Even/Odd/NatWithParity forest spec
  defp nat_parity_forest_poly_spec() do
    alias NockPoly.Fin2ForestPolyF, as: Forest

    Forest.create_forest_spec(
      # Nat has 2 deps (Even and Odd), NatWithParity has 0 deps
      [2, 0],
      [
        # Nat constructors: zero, succ
        [[], [{:base, 0}]],
        # NatWithParity constructors: with_even, with_odd
        # with_even takes a Nat and an Even proof
        [
          [{:base, 0}, {:dep, 0, 0}],
          # with_odd takes a Nat and an Odd proof
          [{:base, 0}, {:dep, 0, 1}]
        ]
      ],
      [
        # Base 0 (Nat) has 2 dependents
        [
          # Even's constructors
          [
            # even_zero (no params)
            [],
            # even_succ (takes Odd)
            [{:dep, 0, 1}]
          ],
          # Odd's constructors
          [
            # odd_succ (takes Even)
            [{:dep, 0, 0}]
          ]
        ],
        # Base 1 (NatWithParity) has no dependent types
        []
      ]
    )
  end

  @doc """
  I demonstrate a forest with dependent types for even and odd proofs.

  This models:
  - Base type 0: Nat (numbers)
  - Base type 1: NatWithParity (a nat with its parity proof)
  - Dependent type 0 on Nat: Even (proof that a nat is even)
  - Dependent type 1 on Nat: Odd (proof that a nat is odd)

  This is an inductive-inductive type because NatWithParity (a base type)
  contains terms of Even and Odd (dependent types).

  mutual
  data FTestNat : Type where
    FTNz : FTestNat
    FTNs : FTestNat -> FTestNat

  data FTestNatWithParity : Type where
    FTNPeven : (n : FTestNat) -> FTestNatEven n -> FTestNatWithParity
    FTNPodd : (n : FTestNat) -> FTestNatOdd n -> FTestNatWithParity

  data FTestNatEven : FTestNat -> Type where
    FTNEvenZero : FTestNatEven FTNz
    FTNEvenSuccOdd : (n : FTestNat) -> FTestNatOdd n -> FTestNatEven (FTNs n)

  data FTestNatOdd : FTestNat -> Type where
    FTNOddSuccEven : (n : FTestNat) -> FTestNatEven n -> FTestNatOdd (FTNs n)
  """
  def forest_nat_even_odd_proof_test() do
    alias NockPoly.Fin2ForestPolyF, as: Forest

    # Get the shared forest spec
    expr_spec = nat_parity_forest_poly_spec()

    assert Forest.validate_forest_spec(expr_spec) == :ok

    # Create nat terms
    zero = Term.com_tv({:base, 0, 0}, [])
    one = Term.com_tv({:base, 0, 1}, [zero])
    two = Term.com_tv({:base, 0, 1}, [one])
    three = Term.com_tv({:base, 0, 1}, [two])

    # Create even/odd proofs
    # Proof that zero is even (Even constructor 0: even_zero)
    even_zero = Term.com_tv({:dep, 0, 0, 0}, [])
    # Proof that one is odd (Odd constructor 0: odd_succ takes even_zero)
    odd_one = Term.com_tv({:dep, 0, 1, 0}, [even_zero])
    # Proof that two is even (Even constructor 1: even_succ takes odd_one)
    even_two = Term.com_tv({:dep, 0, 0, 1}, [odd_one])
    # Proof that three is odd (Odd constructor 0: odd_succ takes even_two)
    odd_three = Term.com_tv({:dep, 0, 1, 0}, [even_two])

    # Create NatWithParity terms (these use the proofs created above)
    # with_even: zero with proof it's even
    zero_with_even = Term.com_tv({:base, 1, 0}, [zero, even_zero])
    # with_odd: one with proof it's odd
    one_with_odd = Term.com_tv({:base, 1, 1}, [one, odd_one])
    # with_even: two with proof it's even
    two_with_even = Term.com_tv({:base, 1, 0}, [two, even_two])
    # with_odd: three with proof it's odd
    three_with_odd = Term.com_tv({:base, 1, 1}, [three, odd_three])

    # Typecheck everything
    assert Forest.typecheck(zero, expr_spec) == {:ok, {:base, 0}}
    assert Forest.typecheck(one, expr_spec) == {:ok, {:base, 0}}
    assert Forest.typecheck(two, expr_spec) == {:ok, {:base, 0}}
    assert Forest.typecheck(three, expr_spec) == {:ok, {:base, 0}}
    assert Forest.typecheck(even_zero, expr_spec) == {:ok, {:dep, 0, 0}}
    assert Forest.typecheck(odd_one, expr_spec) == {:ok, {:dep, 0, 1}}
    assert Forest.typecheck(even_two, expr_spec) == {:ok, {:dep, 0, 0}}
    assert Forest.typecheck(odd_three, expr_spec) == {:ok, {:dep, 0, 1}}
    assert Forest.typecheck(zero_with_even, expr_spec) == {:ok, {:base, 1}}
    assert Forest.typecheck(one_with_odd, expr_spec) == {:ok, {:base, 1}}
    assert Forest.typecheck(two_with_even, expr_spec) == {:ok, {:base, 1}}
    assert Forest.typecheck(three_with_odd, expr_spec) == {:ok, {:base, 1}}

    # Test soundness: invalid proofs should be rejected
    # Try to prove 1 is even using even_succ with wrong proof type (should fail)
    # even_succ expects Odd, but we give it Even
    invalid_one_even = Term.com_tv({:dep, 0, 0, 1}, [even_zero])
    result = Forest.typecheck(invalid_one_even, expr_spec)
    assert {:error, errors} = result

    assert Enum.any?(errors, fn e ->
             match?(
               {:invalid_param_type, {:dep, 0, 0, 1}, 0, {:dep, 0, 0}},
               e
             )
           end)

    # Try to prove 2 is odd using odd_succ with wrong proof type (should fail)
    # odd_succ expects Even, but we give it Odd
    invalid_two_odd = Term.com_tv({:dep, 0, 1, 0}, [odd_one])
    result2 = Forest.typecheck(invalid_two_odd, expr_spec)
    assert {:error, errors2} = result2

    assert Enum.any?(errors2, fn e ->
             match?(
               {:invalid_param_type, {:dep, 0, 1, 0}, 0, {:dep, 0, 1}},
               e
             )
           end)

    # Try to create even_zero with a parameter (should fail - wrong arity)
    invalid_even_zero_with_param = Term.com_tv({:dep, 0, 0, 0}, [zero])
    result3 = Forest.typecheck(invalid_even_zero_with_param, expr_spec)
    assert {:error, errors3} = result3

    assert Enum.any?(errors3, fn e ->
             match?({:invalid_arity, {:dep, 0, 0, 0}, 0, 1}, e)
           end)

    # Try to create NatWithParity with mismatched proof
    # zero with odd proof (should fail)
    invalid_zero_with_odd = Term.com_tv({:base, 1, 1}, [zero, even_zero])
    result4 = Forest.typecheck(invalid_zero_with_odd, expr_spec)
    assert {:error, errors4} = result4

    assert Enum.any?(errors4, fn e ->
             match?({:invalid_param_type, {:base, 1, 1}, 1, {:dep, 0, 0}}, e)
           end)

    # one with even proof (should fail)
    invalid_one_with_even = Term.com_tv({:base, 1, 0}, [one, odd_one])
    result5 = Forest.typecheck(invalid_one_with_even, expr_spec)
    assert {:error, errors5} = result5

    assert Enum.any?(errors5, fn e ->
             match?({:invalid_param_type, {:base, 1, 0}, 1, {:dep, 0, 1}}, e)
           end)

    :ok
  end

  @doc """
  I demonstrate pattern matching/elimination on forest terms using cata and eval.

  This test shows how to:
  - Extract values from terms
  - Transform proof terms
  - Compute over inductive-inductive structures
  - Work with both closed terms (cata) and open terms (eval)
  """
  def forest_elimination_test() do
    alias NockPoly.Fin2ForestPolyF, as: Forest

    # Get the shared forest spec
    expr_spec = nat_parity_forest_poly_spec()

    # Create some terms
    zero = Term.com_tv({:base, 0, 0}, [])
    one = Term.com_tv({:base, 0, 1}, [zero])
    two = Term.com_tv({:base, 0, 1}, [one])

    even_zero = Term.com_tv({:dep, 0, 0, 0}, [])
    odd_one = Term.com_tv({:dep, 0, 1, 0}, [even_zero])
    even_two = Term.com_tv({:dep, 0, 0, 1}, [odd_one])

    zero_with_even = Term.com_tv({:base, 1, 0}, [zero, even_zero])
    one_with_odd = Term.com_tv({:base, 1, 1}, [one, odd_one])

    # Example 1: Extract the natural number value from a Nat term
    nat_value_algebra = fn
      # Base type 0 (Nat) constructors
      # zero
      {:base, 0, 0}, [], _spec -> 0
      # succ
      {:base, 0, 1}, [pred_val], _spec -> pred_val + 1
    end

    # Test extracting values using cata (for closed terms)
    assert Forest.cata(zero, nat_value_algebra, expr_spec) == 0
    assert Forest.cata(one, nat_value_algebra, expr_spec) == 1
    assert Forest.cata(two, nat_value_algebra, expr_spec) == 2

    # Example 2: Extract the nat from a NatWithParity term
    extract_nat_algebra = fn
      # NatWithParity constructors
      # with_even
      {:base, 1, 0}, [nat_val, _proof], _spec -> nat_val
      # with_odd
      {:base, 1, 1}, [nat_val, _proof], _spec -> nat_val
      # For nat constructors, compute their value
      {:base, 0, 0}, [], _spec -> 0
      {:base, 0, 1}, [pred_val], _spec -> pred_val + 1
      # For dep constructors (proofs), we can return 0 as a dummy value
      # since they won't appear in the result when extracting from NatWithParity
      {:dep, _, _, _}, _, _spec -> 0
    end

    assert Forest.cata(zero_with_even, extract_nat_algebra, expr_spec) == 0
    assert Forest.cata(one_with_odd, extract_nat_algebra, expr_spec) == 1

    # Example 3: Count the depth of a proof
    proof_depth_algebra = fn
      # Even/Odd proofs
      # even_zero has depth 0
      {:dep, 0, 0, 0}, [], _spec ->
        0

      # even_succ
      {:dep, 0, 0, 1}, [odd_depth], _spec ->
        odd_depth + 1

      # odd_succ
      {:dep, 0, 1, 0}, [even_depth], _spec ->
        even_depth + 1

      # For nat constructors, take max of children
      {:base, 0, _}, children, _spec ->
        if Enum.empty?(children), do: 0, else: Enum.max(children)
    end

    assert Forest.cata(even_zero, proof_depth_algebra, expr_spec) == 0
    assert Forest.cata(odd_one, proof_depth_algebra, expr_spec) == 1
    assert Forest.cata(even_two, proof_depth_algebra, expr_spec) == 2

    # Test the nat constructor cases
    # Empty children
    assert Forest.cata(zero, proof_depth_algebra, expr_spec) == 0
    # Max of [0]
    assert Forest.cata(one, proof_depth_algebra, expr_spec) == 0
    # Max of [0]
    assert Forest.cata(two, proof_depth_algebra, expr_spec) == 0

    # Example 4: Transform a term - double all nat values
    double_nat_algebra = fn
      # Transform Nat constructors
      {:base, 0, 0}, [], _spec ->
        # zero → zero
        Term.com_tv({:base, 0, 0}, [])

      {:base, 0, 1}, [doubled_pred], _spec ->
        # succ(n) → succ(succ(doubled(n)))
        Term.com_tv({:base, 0, 1}, [
          Term.com_tv({:base, 0, 1}, [doubled_pred])
        ])
    end

    doubled_two = Forest.cata(two, double_nat_algebra, expr_spec)
    # Verify it's 4 by extracting the value
    assert Forest.cata(doubled_two, nat_value_algebra, expr_spec) == 4

    # Example 5: Using eval with open terms containing variables
    # Create an open term: succ(x) where x is a variable
    open_succ_term = Term.com_tv({:base, 0, 1}, [Term.var_tv(:x)])

    # Substitution that replaces :x with the value 3
    substitution = fn
      # We'll substitute the value 3 for variable x
      :x -> 3
    end

    # Evaluate the open term with substitution
    result =
      Forest.eval(open_succ_term, nat_value_algebra, substitution, expr_spec)

    # succ(3) = 4
    assert result == 4

    # Example 6: Transform open terms with variables
    # Create an open term: with_even(x, y) where x and y are variables
    open_parity_term =
      Term.com_tv({:base, 1, 0}, [
        Term.var_tv(:nat_var),
        Term.var_tv(:proof_var)
      ])

    # Algebra that extracts and doubles the nat value
    # This algebra is only applied to base type terms (Nat and NatWithParity)
    # so we don't need cases for dep constructors
    double_extract_algebra = fn
      {:base, 1, 0}, [nat_val, _proof], _spec -> nat_val * 2
      {:base, 1, 1}, [nat_val, _proof], _spec -> nat_val * 2
      {:base, 0, 0}, [], _spec -> 0
      {:base, 0, 1}, [pred_val], _spec -> pred_val + 1
    end

    # Substitution that provides values for our variables
    var_substitution = fn
      # The nat value is 2
      :nat_var -> 2
      # Dummy proof value (algebra ignores it)
      :proof_var -> 0
    end

    # Evaluate the open term
    result2 =
      Forest.eval(
        open_parity_term,
        double_extract_algebra,
        var_substitution,
        expr_spec
      )

    # Doubled the nat value 2 to get 4
    assert result2 == 4

    # Test the other cases in double_extract_algebra
    # Test with_odd constructor
    open_odd_parity =
      Term.com_tv({:base, 1, 1}, [
        Term.var_tv(:nat_var),
        Term.var_tv(:proof_var)
      ])

    result3 =
      Forest.eval(
        open_odd_parity,
        double_extract_algebra,
        var_substitution,
        expr_spec
      )

    # Also doubles the nat value
    assert result3 == 4

    # Test nat constructors directly
    assert Forest.cata(zero, double_extract_algebra, expr_spec) == 0
    assert Forest.cata(one, double_extract_algebra, expr_spec) == 1

    :ok
  end

  @doc """
  I test forest spec validation errors.
  """
  def fin2_forest_poly_spec_validation_errors_test() do
    alias NockPoly.Fin2ForestPolyF, as: Forest

    # Test base_positions length mismatch
    invalid_spec = %Forest{
      # 2 base types
      forest: [1, 0],
      # Only 1 position map
      base_positions: [%{0 => []}],
      dep_positions: [[%{0 => []}], []]
    }

    assert Forest.validate_forest_spec(invalid_spec) ==
             {:error, :base_positions_length_mismatch}

    # Test dep_positions length mismatch
    invalid_spec2 = %Forest{
      # 2 base types
      forest: [1, 0],
      base_positions: [%{}, %{}],
      # Only 1 dep position list
      dep_positions: [[%{0 => []}]]
    }

    assert Forest.validate_forest_spec(invalid_spec2) ==
             {:error, :dep_positions_length_mismatch}

    # Test invalid position specs - wrong number of dep position maps
    invalid_spec3 = %Forest{
      # Base 0 has 1 dep, base 1 has 0 deps
      forest: [1, 0],
      base_positions: [%{0 => []}, %{0 => []}],
      # Base 0 should have 1 dep position map, but we give 2
      dep_positions: [[%{0 => []}, %{0 => []}], []]
    }

    assert Forest.validate_forest_spec(invalid_spec3) ==
             {:error, :invalid_position_specs}

    # Test invalid position specs - invalid forest object in parameter list
    invalid_spec4 = %Forest{
      # 2 base types, first has 1 dep
      forest: [1, 0],
      # Base constructor has invalid parameter reference (base 2 doesn't exist)
      base_positions: [%{0 => [{:base, 2}]}, %{0 => []}],
      dep_positions: [[%{0 => []}], []]
    }

    assert Forest.validate_forest_spec(invalid_spec4) ==
             {:error, :invalid_position_specs}

    # Test invalid position specs - invalid dep reference in parameter list
    invalid_spec5 = %Forest{
      # Base 0 has 1 dep, base 1 has 0 deps
      forest: [1, 0],
      base_positions: [%{0 => []}, %{0 => []}],
      # Dep constructor references invalid dep (base 0 only has dep 0, not dep 1)
      dep_positions: [[%{0 => [{:dep, 0, 1}]}], []]
    }

    assert Forest.validate_forest_spec(invalid_spec5) ==
             {:error, :invalid_position_specs}

    :ok
  end

  @doc """
  I test get_ctor_params error cases.
  """
  def fin2_forest_get_ctor_params_errors_test() do
    alias NockPoly.Fin2ForestPolyF, as: Forest

    spec = Forest.simple_forest_spec([0, 1])

    # Test invalid type index for base constructor
    assert Forest.get_ctor_params(spec, {:base, 2, 0}) ==
             {:error, :invalid_type_index}

    # Test invalid base index for dependent constructor
    assert Forest.get_ctor_params(spec, {:dep, 2, 0, 0}) ==
             {:error, :invalid_base_index}

    assert Forest.get_ctor_params(spec, {:base, 0, 5}) ==
             {:error, :invalid_ctor_index}

    dep_spec =
      Forest.create_forest_spec(
        [1],
        [[[]]],
        [[[[{:base, 0}]]]]
      )

    assert Forest.get_ctor_params(dep_spec, {:dep, 0, 0, 5}) ==
             {:error, :invalid_ctor_index}

    # Test invalid dep index for dependent constructor
    # Base 0 has only 1 dependent (at index 0), so index 1 is invalid
    assert Forest.get_ctor_params(dep_spec, {:dep, 0, 1, 0}) ==
             {:error, :invalid_dep_index}

    :ok
  end

  @doc """
  I test typecheck with child errors to ensure error propagation.
  """
  def fin2_forest_typecheck_child_errors_test() do
    alias NockPoly.Fin2ForestPolyF, as: Forest

    spec = Forest.simple_forest_spec([1, 1])

    # Create a term with an invalid child that will propagate errors
    # Constructor 5 doesn't exist, so this will create an error
    invalid_child = Term.com_tv({:base, 0, 5}, [])

    # Use the invalid child in a valid constructor
    parent_term = Term.com_tv({:base, 0, 1}, [invalid_child])

    {:error, errors} = Forest.typecheck(parent_term, spec)

    # Verify we get the propagated error from the child
    assert Enum.any?(errors, fn e ->
             match?({:invalid_constructor, {:base, 0, 5}}, e)
           end)

    :ok
  end

  @doc """
  I test typecheck with variables.
  """
  def fin2_forest_typecheck_variable_test() do
    alias NockPoly.Fin2ForestPolyF, as: Forest

    spec = Forest.simple_forest_spec([0, 1])

    # Create a term with a variable
    var_term = Term.var_tv(:x)

    {:error, errors} = Forest.typecheck(var_term, spec)

    assert errors == [{:invalid_variable, :x}]

    :ok
  end
end
