defmodule Examples.ENockPoly.EFinIndIndPolyF do
  use Memoize

  import ExUnit.Assertions
  import NockPoly.Term.MacroDefs
  alias NockPoly.Term
  alias NockPoly.FinIndIndPolyF, as: IndIndF

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
      %IndIndF.RepresentableNt{
        base_field_map: [],
        dep_field_maps: []
      },
      # For dependent constructor 1 (maps to base constructor 1)
      %IndIndF.RepresentableNt{
        base_field_map: [0, 1],
        dep_field_maps: [[], []]
      },
      # For dependent constructor 2 (maps to base constructor 2)
      %IndIndF.RepresentableNt{
        base_field_map: [0, 1, 2],
        dep_field_maps: [[0], [0], [0]]
      },
      # For dependent constructor 3 (maps to base constructor 2)
      %IndIndF.RepresentableNt{
        base_field_map: [0, 0, 1],
        # Corrected the last entry to [0]
        dep_field_maps: [[0], [0], [0]]
      }
    ]

    # Assemble the complete natural transformation
    stnt = %IndIndF.IndIndF1Nt{
      pos_map: stnt_pos_map,
      rep_transformations: st_rep_transformations
    }

    # Create the slice relating the dependent type to the base type
    stf1sl = %IndIndF.IndIndF1Slice{
      total: st1f_f1,
      projection: stnt
    }

    # Create the complete inductive-inductive type system
    stmlf = %IndIndF.IndIndF{
      base: st0f_f1,
      slice: stf1sl
    }

    # Validate the spec before using it
    assert :ok = IndIndF.validate_ind_ind_f(stmlf)

    # Create and verify terms of the base type
    # Using {:base, pos} constructor format to explicitly tag the type

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

    nested_term =
      tvc({:base, 2}, [
        base0_term,
        base0_term,
        base0_term,
        dep0_term,
        dep0_term,
        dep0_term
      ])

    assert {:ok, 0} = IndIndF.typecheck(nested_term, stmlf)

    # Test an invalid term with a non-existent constructor
    invalid_term = tvc0({:base, 3})

    assert match?(
             {:error, [{:invalid_constructor, _}]},
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
             {:error, [{:invalid_constructor_format, _, _}]},
             IndIndF.typecheck(invalid_format_term, stmlf)
           )

    nested_term
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
    assert {:error, [:invalid_mapping_length]} =
             Term.validate_fin_mapping([0, 1], 3, 3)

    # Mapping out of range
    assert {:error, [:mapping_out_of_range]} =
             Term.validate_fin_mapping([0, 3, 1], 3, 3)

    # Create base specs for further validation testing
    # Remember: in a natural transformation, we map from dependent (source) to base (target)
    # Source with 2 base fields, each with 1 dependent field
    source_rep = [1, 1]
    # Target with 2 base fields, no dependent fields (it's the base type)
    target_rep = [0, 0]

    # Valid representable NT
    valid_rep_nt = %IndIndF.RepresentableNt{
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
    invalid_rep_nt1 = %IndIndF.RepresentableNt{
      base_field_map: [0],
      dep_field_maps: [[0], []]
    }

    assert {:error, [:invalid_mapping_length]} =
             IndIndF.validate_representable_nt(
               invalid_rep_nt1,
               source_rep,
               target_rep
             )

    # Invalid dep field maps length
    invalid_rep_nt2 = %IndIndF.RepresentableNt{
      base_field_map: [0, 1],
      # Only one dep map but need two
      dep_field_maps: [[]]
    }

    assert {:error, [:invalid_dep_field_maps_length]} =
             IndIndF.validate_representable_nt(
               invalid_rep_nt2,
               source_rep,
               target_rep
             )

    # Invalid dep field mapping (out of range)
    # Use different source/target to have actual dependent fields to test
    source_rep_with_deps = [2, 1]
    target_rep_with_deps = [1, 1]

    invalid_rep_nt3 = %IndIndF.RepresentableNt{
      base_field_map: [0, 1],
      # First dep map has out-of-range index (2 >= source dep count of 2)
      dep_field_maps: [[2], [0]]
    }

    assert {:error, [:mapping_out_of_range]} =
             IndIndF.validate_representable_nt(
               invalid_rep_nt3,
               source_rep_with_deps,
               target_rep_with_deps
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
    valid_ind_f1_nt = %IndIndF.IndIndF1Nt{
      # Source pos 0 -> Target pos 0, Source pos 1 -> Target pos 1
      pos_map: [0, 1],
      rep_transformations: [
        # For source pos 0 -> target pos 0
        %IndIndF.RepresentableNt{
          # Target base field 0 -> Source base field 0
          base_field_map: [0],
          # No dep fields for target
          dep_field_maps: [[]]
        },
        # For source pos 1 -> target pos 1
        %IndIndF.RepresentableNt{
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
    invalid_ind_f1_nt1 = %IndIndF.IndIndF1Nt{
      # Too short
      pos_map: [0],
      rep_transformations: [
        %IndIndF.RepresentableNt{
          base_field_map: [0],
          dep_field_maps: [[]]
        }
      ]
    }

    assert {:error, [:invalid_mapping_length]} =
             IndIndF.validate_ind_ind_f1_nt(
               invalid_ind_f1_nt1,
               source_ind_f1,
               target_ind_f1
             )

    # Invalid rep_transformations length
    invalid_ind_f1_nt2 = %IndIndF.IndIndF1Nt{
      pos_map: [0, 1],
      rep_transformations: [
        %IndIndF.RepresentableNt{
          base_field_map: [0],
          dep_field_maps: [[]]
        }
        # Missing the second transformation
      ]
    }

    assert {:error, [:invalid_rep_transformations_length]} =
             IndIndF.validate_ind_ind_f1_nt(
               invalid_ind_f1_nt2,
               source_ind_f1,
               target_ind_f1
             )

    # Invalid rep_transformation internal mapping
    invalid_ind_f1_nt3 = %IndIndF.IndIndF1Nt{
      pos_map: [0, 1],
      rep_transformations: [
        # Valid first transformation
        %IndIndF.RepresentableNt{
          base_field_map: [0],
          dep_field_maps: [[]]
        },
        # Invalid second transformation - dep map out of range
        %IndIndF.RepresentableNt{
          base_field_map: [0, 1],
          # First dep map has out-of-range index (5 >= source dep count)
          dep_field_maps: [[5], [0]]
        }
      ]
    }

    assert {:error, [:mapping_out_of_range]} =
             IndIndF.validate_ind_ind_f1_nt(
               invalid_ind_f1_nt3,
               source_ind_f1,
               target_ind_f1
             )

    # Setup for slice validation
    # Two constructors with no dep fields
    base_type = [[], [0, 0]]

    # The dependent type needs consistent dep field counts
    # Two constructors with 0 and 2 base fields, and dep fields
    dep_type = [[], [1, 1]]

    valid_slice = %IndIndF.IndIndF1Slice{
      total: dep_type,
      projection: %IndIndF.IndIndF1Nt{
        pos_map: [0, 1],
        rep_transformations: [
          %IndIndF.RepresentableNt{
            base_field_map: [],
            # First constructor has no base fields
            dep_field_maps: []
          },
          %IndIndF.RepresentableNt{
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
    invalid_slice = %IndIndF.IndIndF1Slice{
      # Only one constructor, causing mismatch with projection
      total: [[]],
      # Projection expects two constructors
      projection: valid_slice.projection
    }

    assert {:error, [:invalid_mapping_length]} =
             IndIndF.validate_ind_ind_f1_slice(invalid_slice, base_type)

    # Test validate_ind_ind_f
    valid_ind_ind_f = %IndIndF.IndIndF{
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
    projection = %IndIndF.IndIndF1Nt{
      # Straightforward mapping
      pos_map: [0, 1],
      rep_transformations: [
        %IndIndF.RepresentableNt{
          base_field_map: [],
          dep_field_maps: []
        },
        %IndIndF.RepresentableNt{
          base_field_map: [0, 1],
          dep_field_maps: [[0], [0]]
        }
      ]
    }

    # Complete system
    stmlf = %IndIndF.IndIndF{
      base: base_type,
      slice: %IndIndF.IndIndF1Slice{
        total: dep_type,
        projection: projection
      }
    }

    # Valid terms for testing
    base0_term = tvc0({:base, 0})
    dep0_term = tvc0({:dep, 0})

    # Test field count errors in base constructor
    # Should have 2 fields
    invalid_field_count_base = tvc({:base, 1}, [base0_term])

    assert {:error, [{:invalid_field_count, _, 2, 1}]} =
             IndIndF.typecheck(invalid_field_count_base, stmlf)

    # Test field count errors in dependent constructor
    # Should have 4 fields (2 base + 2 dep)
    invalid_field_count_dep = tvc({:dep, 1}, [base0_term])

    assert {:error, [{:invalid_field_count, _, 4, 1}]} =
             IndIndF.typecheck(invalid_field_count_dep, stmlf)

    # Create a valid constructor with fields of correct type
    valid_base_term = tvc({:base, 1}, [base0_term, base0_term])
    assert {:ok, 0} = IndIndF.typecheck(valid_base_term, stmlf)

    # Create a term with mixed base and dependent type terms
    # This should fail type checking because dep0_term is not allowed as a field
    # in a base type constructor
    invalid_type_term = tvc({:base, 1}, [base0_term, dep0_term])
    {:error, errors} = IndIndF.typecheck(invalid_type_term, stmlf)

    # Verify errors list contains an invalid_field_type error
    assert Enum.any?(errors, fn e ->
             match?({:invalid_field_type, _, _, _}, e)
           end)

    # Test invalid term format - using a term with an invalid structure
    # but still with the proper tagged variant format
    invalid_term_format = {:in_tv, {:tcom, {:unknown_format, []}}}

    assert match?(
             {:error, [{:invalid_constructor_format, _, _}]},
             IndIndF.typecheck(invalid_term_format, stmlf)
           )

    # Test error propagation with check_fields function
    # Create an invalid field that will fail typecheck
    # Constructor index 5 doesn't exist
    invalid_field = tvc0({:base, 5})

    # Use the invalid field in a valid constructor
    term_with_invalid_field =
      tvc({:base, 1}, [invalid_field, invalid_field])

    {:error, field_errors} = IndIndF.typecheck(term_with_invalid_field, stmlf)

    assert Enum.any?(field_errors, fn e ->
             match?({:invalid_constructor, _}, e)
           end)

    stmlf
  end
end
