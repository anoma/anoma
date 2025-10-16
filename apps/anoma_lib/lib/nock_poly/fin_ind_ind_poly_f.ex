defmodule NockPoly.FinIndIndPolyF do
  use TypedStruct

  @moduledoc """
  I implement finitary inductive-inductive polynomial functors, which enable
  the definition of mutually dependent types where one depends on the constructors
  of the other.

  Inductive-inductive types consist of multiple type definitions where:
  1. The second type depends on the first type
  2. Each type can refer to both itself and the other type
  3. The second type can "see" the constructors of the first type

  This module extends the concepts from FinPolyF and FinSlicePolyF to support
  these type dependencies.
  """

  alias NockPoly.Term

  @typedoc """
  I represent a single position (constructor) in an inductive-inductive type,
  specifying how many fields of each type it has.

  I am a list of non-negative integers where:
  - Length = number of fields of base type
  - Each value = number of dependent type fields for the corresponding base type field

  Example: [0, 2, 1] means:
  - 3 fields of base type
  - 0 dependent fields for the first base field
  - 2 dependent fields for the second base field
  - 1 dependent field for the third base field
  """
  @type ind_ind_f1_rep :: [non_neg_integer()]

  @typedoc """
  I represent a complete inductive polynomial functor (IndIndF1).

  I am a list of position representations (IndIndF1Rep) where:
  - Length = number of positions (constructors)
  - Each element = representation of that position's field structure

  Example: [[0, 1], [2, 0], []] means:
  - 3 positions (constructors)
  - First position has 2 base fields, with 0 and 1 dependent fields respectively
  - Second position has 2 base fields, with 2 and 0 dependent fields respectively
  - Third position has 0 base fields (and thus no dependent fields)
  """
  @type ind_ind_f1 :: [ind_ind_f1_rep()]

  typedstruct module: RepresentableNt, enforce: true do
    @typedoc """
    I represent a natural transformation from a representable functor.
    I consist of:

    * `base_field_map` - Maps target base fields to source base fields:
      - A mapping from target field indices to source field indices
      - Length equals number of base fields in target
      - Each element is a valid field index in source

    * `dep_field_maps` - Dependent field mappings:
      - A list of mappings, one for each base field in source
      - Each mapping maps target dependent fields to source dependent fields
      - Length of inner mapping equals number of dependent fields for corresponding target field
      - Each element is a valid dependent field index for the corresponding source field
    """

    field(:base_field_map, Term.fin_mapping())
    field(:dep_field_maps, [Term.fin_mapping()])
  end

  @type representable_nt :: RepresentableNt.t()

  typedstruct module: IndIndF1Nt, enforce: true do
    @typedoc """
    I represent a natural transformation between IndIndF1 structures.
    I consist of:

    * `pos_map` - Maps source positions to target positions:
      - A mapping from source position indices to target position indices
      - Length equals number of positions in source
      - Each element is a valid position index in target

    * `rep_transformations` - Transformations for each source position:
      - A list of representable natural transformations, one for each source position
      - Each transformation maps from the target position's fields to the source position's fields
      - Length equals number of positions in source
    """

    field(:pos_map, Term.fin_mapping())
    field(:rep_transformations, [NockPoly.FinIndIndPolyF.representable_nt()])
  end

  @type ind_ind_f1_nt :: IndIndF1Nt.t()

  typedstruct module: IndIndF1Slice, enforce: true do
    @typedoc """
    I represent a slice over an IndIndF1, which is a pair of another IndIndF1
    and a natural transformation to the base IndIndF1.
    """

    field(:total, NockPoly.FinIndIndPolyF.ind_ind_f1())
    field(:projection, NockPoly.FinIndIndPolyF.ind_ind_f1_nt())
  end

  @type ind_ind_f1_slice :: IndIndF1Slice.t()

  typedstruct module: IndIndF, enforce: true do
    @typedoc """
    I represent a complete finitary inductive-inductive polynomial functor.

    I consist of a base IndIndF1 and a slice over it (the dependent part).
    """

    field(:base, NockPoly.FinIndIndPolyF.ind_ind_f1())
    field(:slice, NockPoly.FinIndIndPolyF.ind_ind_f1_slice())
  end

  @type ind_ind_f :: IndIndF.t()

  @doc """
  I validate a natural transformation from a representable functor.
  """
  @spec validate_representable_nt(
          representable_nt(),
          # source
          ind_ind_f1_rep(),
          # target
          ind_ind_f1_rep()
        ) :: :ok | {:error, atom()}
  def validate_representable_nt(
        %{
          base_field_map: base_field_map,
          dep_field_maps: dep_field_maps
        },
        source_rep,
        target_rep
      ) do
    source_base_field_count = length(source_rep)
    target_base_field_count = length(target_rep)

    # Validate base field mapping
    with :ok <-
           Term.validate_fin_mapping(
             base_field_map,
             target_base_field_count,
             source_base_field_count
           ) do
      # Validate dependent field mappings
      if length(dep_field_maps) != target_base_field_count do
        {:error, :invalid_dep_field_maps_length}
      else
        # Check each dependent field mapping
        dep_mapping_results =
          Enum.with_index(dep_field_maps)
          |> Enum.map(fn {dep_map, target_idx} ->
            # Get the source field this target field maps to
            source_idx = Enum.at(base_field_map, target_idx)

            # Get dependent field counts for target and source
            target_dep_count = Enum.at(target_rep, target_idx)
            source_dep_count = Enum.at(source_rep, source_idx)

            # Validate this dependent field mapping using fin_mapping
            Term.validate_fin_mapping(
              dep_map,
              target_dep_count,
              source_dep_count
            )
          end)

        # Check for any errors in dependent mappings
        errors = Enum.filter(dep_mapping_results, &(&1 != :ok))

        if Enum.empty?(errors) do
          :ok
        else
          {:error, {:invalid_dependent_mappings, errors}}
        end
      end
    end
  end

  @doc """
  I validate a natural transformation between IndIndF1 instances.
  """
  @spec validate_ind_ind_f1_nt(ind_ind_f1_nt(), ind_ind_f1(), ind_ind_f1()) ::
          :ok | {:error, atom()}
  def validate_ind_ind_f1_nt(
        %{
          pos_map: pos_map,
          rep_transformations: rep_transformations
        },
        source,
        target
      ) do
    source_pos_count = length(source)
    target_pos_count = length(target)

    # Validate position mapping
    with :ok <-
           Term.validate_fin_mapping(
             pos_map,
             source_pos_count,
             target_pos_count
           ) do
      # Validate length of rep_transformations
      if length(rep_transformations) != source_pos_count do
        {:error, :invalid_rep_transformations_length}
      else
        # Validate each representable transformation
        rep_nt_results =
          Enum.with_index(rep_transformations)
          |> Enum.map(fn {rep_nt, source_idx} ->
            source_rep = Enum.at(source, source_idx)
            target_idx = Enum.at(pos_map, source_idx)
            target_rep = Enum.at(target, target_idx)

            validate_representable_nt(rep_nt, source_rep, target_rep)
          end)

        # Check for any errors in representable transformations
        errors = Enum.filter(rep_nt_results, &(&1 != :ok))

        if Enum.empty?(errors) do
          :ok
        else
          {:error, {:invalid_representable_transformations, errors}}
        end
      end
    end
  end

  @doc """
  I validate an IndIndF1Slice to ensure it's well-formed.
  """
  @spec validate_ind_ind_f1_slice(
          ind_ind_f1_slice(),
          ind_ind_f1()
        ) ::
          :ok | {:error, atom()}
  def validate_ind_ind_f1_slice(%{total: total, projection: projection}, base) do
    validate_ind_ind_f1_nt(projection, total, base)
  end

  @doc """
  I validate a complete IndIndF to ensure all its parts are well-formed.
  """
  @spec validate_ind_ind_f(ind_ind_f()) :: :ok | {:error, atom()}
  def validate_ind_ind_f(%{base: base, slice: slice}) do
    validate_ind_ind_f1_slice(slice, base)
  end

  @doc """
  I check if a term has a valid type according to the inductive-inductive typespec.

  The term constructor must be either {:base, pos} or {:dep, pos} to explicitly
  indicate which type it belongs to.

  Returns either {:ok, type_index} or {:error, reason}.
  """
  @spec typecheck(Term.tv(any(), none()), ind_ind_f()) ::
          {:ok, non_neg_integer()} | {:error, any()}
  def typecheck(term, ind_ind_f) do
    case Term.out_tv(term) do
      {:tcom, {{:base, pos}, fields}} ->
        typecheck_base_constructor(pos, fields, ind_ind_f)

      {:tcom, {{:dep, pos}, fields}} ->
        typecheck_dep_constructor(pos, fields, ind_ind_f)

      {:tcom, {ctor, _}} ->
        {:error,
         {:invalid_constructor_format, ctor,
          "Expected {:base, pos} or {:dep, pos}"}}
    end
  end

  @doc """
  I validate a term with a base type constructor.

  Returns either {:ok, 0} (for base type) or {:error, reason}.
  """
  @spec typecheck_base_constructor(
          non_neg_integer(),
          [Term.tv(any(), any())],
          ind_ind_f()
        ) ::
          {:ok, 0} | {:error, any()}
  def typecheck_base_constructor(pos, fields, ind_ind_f) do
    %{base: base} = ind_ind_f
    positions = length(base)

    if pos < 0 or pos >= positions do
      {:error, {:invalid_constructor, {:base, pos}}}
    else
      # Get expected field count for this constructor
      ctor_rep = Enum.at(base, pos)
      base_field_count = length(ctor_rep)
      dep_field_count = Enum.sum(ctor_rep)
      expected_field_count = base_field_count + dep_field_count

      if length(fields) != expected_field_count do
        {:error,
         {:invalid_field_count, {:base, pos}, expected_field_count,
          length(fields)}}
      else
        # Extract base and dependent fields
        base_fields = Enum.take(fields, base_field_count)
        dep_fields = Enum.drop(fields, base_field_count)

        # Check fields and ensure they have the correct types
        with :ok <- check_fields(base_fields, ind_ind_f, 0),
             :ok <- check_fields(dep_fields, ind_ind_f, 1) do
          {:ok, 0}
        end
      end
    end
  end

  @doc """
  I validate a term with a dependent type constructor.

  Returns either {:ok, 1} (for dependent type) or {:error, reason}.
  """
  @spec typecheck_dep_constructor(
          non_neg_integer(),
          [Term.tv(any(), any())],
          ind_ind_f()
        ) ::
          {:ok, 1} | {:error, any()}
  def typecheck_dep_constructor(pos, fields, ind_ind_f) do
    %{slice: %{total: dep_type}} = ind_ind_f
    positions = length(dep_type)

    if pos < 0 or pos >= positions do
      {:error, {:invalid_constructor, {:dep, pos}}}
    else
      # Get expected field count for this constructor
      ctor_rep = Enum.at(dep_type, pos)
      base_field_count = length(ctor_rep)
      dep_field_count = Enum.sum(ctor_rep)
      expected_field_count = base_field_count + dep_field_count

      if length(fields) != expected_field_count do
        {:error,
         {:invalid_field_count, {:dep, pos}, expected_field_count,
          length(fields)}}
      else
        # Extract base and dependent fields
        base_fields = Enum.take(fields, base_field_count)
        dep_fields = Enum.drop(fields, base_field_count)

        # Check fields and ensure they have the correct types
        with :ok <- check_fields(base_fields, ind_ind_f, 0),
             :ok <- check_fields(dep_fields, ind_ind_f, 1) do
          {:ok, 1}
        end
      end
    end
  end

  # Helper function to check if a list of fields has the correct types.
  # This ensures that base fields can only be base terms and dependent fields can only be dependent terms.
  @spec check_fields(
          [Term.tv(any(), any())],
          ind_ind_f(),
          # expected_type: 0 = base, 1 = dependent
          non_neg_integer()
        ) :: :ok | {:error, any()}
  defp check_fields(fields, ind_ind_f, expected_type) do
    # If no fields to check, return :ok immediately
    if Enum.empty?(fields) do
      :ok
    else
      field_check_results =
        Enum.map(fields, fn field ->
          # Check each field using the main typecheck function
          # which will dispatch based on the constructor tag
          case typecheck(field, ind_ind_f) do
            {:ok, actual_type} ->
              if actual_type == expected_type do
                :ok
              else
                # Field has valid type but not the right one for this position
                {:error,
                 {:invalid_field_type, field, expected_type, actual_type}}
              end

            error ->
              error
          end
        end)

      errors = Enum.filter(field_check_results, &(&1 != :ok))

      if Enum.empty?(errors) do
        :ok
      else
        {:error, {:invalid_fields, errors}}
      end
    end
  end
end
