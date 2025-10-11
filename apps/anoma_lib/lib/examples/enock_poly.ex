defmodule Examples.ENockPoly do
  use Memoize

  import ExUnit.Assertions
  import NockPoly
  alias NockPoly.Term, as: Term
  import NockPoly.Term.MacroDefs
  alias NockPoly.FinIndIndPolyF, as: IndIndF
  alias NockPoly.NockTerms
  alias Noun

  use TypedStruct

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
    base0_term = tvc0({:base, 0})
    assert {:ok, 0} = IndIndF.typecheck(base0_term, stmlf)

    # Base type constructor 1: two base type parameters
    base1_term = tvc({:base, 1}, [base0_term, base0_term])
    assert {:ok, 0} = IndIndF.typecheck(base1_term, stmlf)

    # Create and verify terms of the dependent type
    # Using {:dep, pos} constructor format to explicitly tag the type

    # Dependent type constructor 0: no parameters
    dep0_term = tvc0({:dep, 0})
    assert {:ok, 1} = IndIndF.typecheck(dep0_term, stmlf)

    # Dependent type constructor 1: two base fields and two dependent fields
    # First two fields are base type, next two are dependent type (1 for each base field)
    dep1_term =
      tvc({:dep, 1}, [base0_term, base0_term, dep0_term, dep0_term])

    assert {:ok, 1} = IndIndF.typecheck(dep1_term, stmlf)

    # Create a more complex term that uses both types
    # Base constructor 2: three base fields, each with one dependent field
    complex_term =
      tvc({:base, 2}, [
        base0_term,
        base0_term,
        base0_term,
        dep0_term,
        dep0_term,
        dep0_term
      ])

    assert {:ok, 0} = IndIndF.typecheck(complex_term, stmlf)

    # Test an invalid term with a non-existent constructor
    invalid_term = tvc0({:base, 3})

    assert match?(
             {:error, {:invalid_constructor, _}},
             IndIndF.typecheck(invalid_term, stmlf)
           )

    # Test an invalid format (using plain integer instead of tagged constructor)
    invalid_format_term =
      tvc(2, [
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
    assert :ok = Term.validate_fin_mapping([0, 1, 2], 3, 3)

    # Invalid mapping length
    assert {:error, :invalid_mapping_length} =
             Term.validate_fin_mapping([0, 1], 3, 3)

    # Mapping out of range
    assert {:error, :mapping_out_of_range} =
             Term.validate_fin_mapping([0, 3, 1], 3, 3)

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
    base0_term = tvc0({:base, 0})
    dep0_term = tvc0({:dep, 0})

    # Test field count errors in base constructor
    # Should have 2 fields
    invalid_field_count_base = tvc({:base, 1}, [base0_term])

    assert {:error, {:invalid_field_count, _, 2, 1}} =
             IndIndF.typecheck(invalid_field_count_base, stmlf)

    # Test field count errors in dependent constructor
    # Should have 4 fields (2 base + 2 dep)
    invalid_field_count_dep = tvc({:dep, 1}, [base0_term])

    assert {:error, {:invalid_field_count, _, 4, 1}} =
             IndIndF.typecheck(invalid_field_count_dep, stmlf)

    # Create a valid constructor with fields of correct type
    valid_base_term = tvc({:base, 1}, [base0_term, base0_term])
    assert {:ok, 0} = IndIndF.typecheck(valid_base_term, stmlf)

    # Create a term with mixed base and dependent type terms
    # This should fail type checking because dep0_term is not allowed as a field
    # in a base type constructor
    invalid_type_term = tvc({:base, 1}, [base0_term, dep0_term])
    {:error, error_info} = IndIndF.typecheck(invalid_type_term, stmlf)

    # Verify the error is of the form {:invalid_fields, _}
    assert match?({:invalid_fields, _}, error_info)

    # Test invalid term format - using a term with an invalid structure
    # but still with the proper tagged variant format
    invalid_term_format = {:in_tv, {:tcom, {:unknown_format, []}}}

    assert match?(
             {:error, {:invalid_constructor_format, _, _}},
             IndIndF.typecheck(invalid_term_format, stmlf)
           )

    # Test error propagation with check_fields function
    # Create an invalid field that will fail typecheck
    # Constructor index 5 doesn't exist
    invalid_field = tvc0({:base, 5})

    # Use the invalid field in a valid constructor
    term_with_invalid_field =
      tvc({:base, 1}, [invalid_field, invalid_field])

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
    alias Term

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
    alias Term

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
    alias Term

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
    alias Term

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
    invalid_spec = %{
      # 2 base types
      forest: [1, 0],
      # Only 1 position map
      base_positions: [%{0 => []}],
      dep_positions: [[%{0 => []}], []]
    }

    assert Forest.validate_forest_spec(invalid_spec) ==
             {:error, :base_positions_length_mismatch}

    # Test dep_positions length mismatch
    invalid_spec2 = %{
      # 2 base types
      forest: [1, 0],
      base_positions: [%{}, %{}],
      # Only 1 dep position list
      dep_positions: [[%{0 => []}]]
    }

    assert Forest.validate_forest_spec(invalid_spec2) ==
             {:error, :dep_positions_length_mismatch}

    # Test invalid position specs - wrong number of dep position maps
    invalid_spec3 = %{
      # Base 0 has 1 dep, base 1 has 0 deps
      forest: [1, 0],
      base_positions: [%{0 => []}, %{0 => []}],
      # Base 0 should have 1 dep position map, but we give 2
      dep_positions: [[%{0 => []}, %{0 => []}], []]
    }

    assert Forest.validate_forest_spec(invalid_spec3) ==
             {:error, :invalid_position_specs}

    # Test invalid position specs - invalid forest object in parameter list
    invalid_spec4 = %{
      # 2 base types, first has 1 dep
      forest: [1, 0],
      # Base constructor has invalid parameter reference (base 2 doesn't exist)
      base_positions: [%{0 => [{:base, 2}]}, %{0 => []}],
      dep_positions: [[%{0 => []}], []]
    }

    assert Forest.validate_forest_spec(invalid_spec4) ==
             {:error, :invalid_position_specs}

    # Test invalid position specs - invalid dep reference in parameter list
    invalid_spec5 = %{
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
    alias Term

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
    alias Term

    spec = Forest.simple_forest_spec([0, 1])

    # Create a term with a variable
    var_term = Term.var_tv(:x)

    {:error, errors} = Forest.typecheck(var_term, spec)

    assert errors == [{:invalid_variable, :x}]

    :ok
  end
end
