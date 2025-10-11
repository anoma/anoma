defmodule NockPoly do
  @moduledoc """
  Representations of polynomial functors in Elixir, and of Nock
  as a polynomial functor.
  """

  require Noun

  use TypedStruct

  alias NockPoly.Term
  alias NockPoly.FinPolyF

  defmodule GenericTerm do
    @moduledoc """
    I provide a generic term representation for typechecking against unified
    polynomial functors.

    I support three kinds of constructors:
    - `:coprod` - Represents a coproduct (sum type) with an index and a term
    - `:prod` - Represents a product type with a list of component terms
    - `:nat` - Represents a natural number literal

    This representation allows building complex nested structures without
    having to encode everything arithmetically as natural numbers. Terms
    of this type can be typechecked against both slice polynomial functors
    and inductive-inductive types.
    """

    alias Term
    alias FinPolyF

    @typedoc """
    The constructor types for generic terms.

    - `{:coprod, index}` - A coproduct with the given index (which constructor)
    - `:prod` - A product of terms
    - `{:nat, value}` - A natural number literal
    """
    @type generic_ctor ::
            {:coprod, non_neg_integer()}
            | :prod
            | {:nat, non_neg_integer()}

    @typedoc "A closed generic term."
    @type generic_term :: Term.t(generic_ctor)

    @typedoc "An open generic term with variables of type `v`."
    @type generic_term_v(v) :: Term.tv(generic_ctor, v)

    @doc """
    Creates a coproduct term with the given index and content.

    The index indicates which constructor of the coproduct is being used.
    Coproduct terms always have exactly one child term.
    """
    @spec coprod(non_neg_integer(), generic_term) :: generic_term
    def coprod(index, content) when is_integer(index) and index >= 0 do
      Term.com_tv({:coprod, index}, [content])
    end

    @doc """
    Creates a product term with the given components.

    Product terms can have any number of components (including zero for
    the unit type).
    """
    @spec prod([generic_term]) :: generic_term
    def prod(components) when is_list(components) do
      Term.com_tv(:prod, components)
    end

    @doc """
    Creates a natural number term with the given value.

    Natural number terms have no children - the value is stored in the
    constructor itself.
    """
    @spec nat(non_neg_integer()) :: generic_term
    def nat(value) when is_integer(value) and value >= 0 do
      Term.com_tv({:nat, value}, [])
    end

    @doc """
    A typespec function for generic terms that validates constructor usage.

    - Coproduct constructors must have exactly 1 child
    - Product constructors can have any number of children
    - Natural number constructors must have exactly 0 children
    """
    @spec generic_tspec(term()) ::
            {:ok, non_neg_integer() | :variable_arity}
            | {:invalid_constructor}
    def generic_tspec({:coprod, index})
        when is_integer(index) and index >= 0 do
      {:ok, 1}
    end

    def generic_tspec(:prod) do
      # Products can have any arity, so we return a special marker
      # that the typecheck function should handle differently
      {:ok, :variable_arity}
    end

    def generic_tspec({:nat, value}) when is_integer(value) and value >= 0 do
      {:ok, 0}
    end

    def generic_tspec(_) do
      {:invalid_constructor}
    end

    @doc """
    Typechecks a generic term to ensure it's well-formed.

    This only checks the structure of the term itself, not whether it
    conforms to any particular type specification.
    """
    @spec typecheck(generic_term) ::
            FinPolyF.check_result(generic_ctor, none())
    def typecheck(term) do
      # We need a custom typecheck because products have variable arity
      Term.cata(term, &typecheck_algebra/1)
    end

    # Helper algebra for typechecking
    @spec typecheck_algebra(
            {generic_ctor, [FinPolyF.check_result(generic_ctor, none())]}
          ) ::
            FinPolyF.check_result(generic_ctor, none())
    defp typecheck_algebra({ctor, child_results}) do
      # First collect any errors from children
      child_errors =
        child_results
        |> Enum.flat_map(fn
          :ok -> []
          {:error, errs} -> errs
        end)

      # Then check this level
      level_result =
        case {ctor, length(child_results)} do
          {{:coprod, _index}, 1} ->
            :ok

          {{:coprod, index}, count} ->
            {:error, [{:invalid_arity, {:coprod, index}, 1, count}]}

          {:prod, _count} ->
            # Products can have any arity
            :ok

          {{:nat, _value}, 0} ->
            :ok

          {{:nat, value}, count} ->
            {:error, [{:invalid_arity, {:nat, value}, 0, count}]}

          {ctor, _} ->
            {:error, [{:invalid_constructor, ctor}]}
        end

      # Combine errors
      all_errors =
        case level_result do
          :ok -> child_errors
          {:error, errs} -> errs ++ child_errors
        end

      if all_errors == [] do
        :ok
      else
        {:error, all_errors}
      end
    end

    @doc """
    Converts a generic term to a simple string representation for debugging.

    Examples:
    - `nat(5)` → `"5"`
    - `prod([nat(1), nat(2)])` → `"(1, 2)"`
    - `coprod(0, nat(3))` → `"inl(3)"`
    - `coprod(1, nat(4))` → `"inr(4)"`
    """
    @spec to_string(generic_term) :: String.t()
    def to_string(term) do
      algebra = fn {ctor, children} ->
        case ctor do
          {:nat, value} ->
            Integer.to_string(value)

          :prod ->
            "(" <> Enum.join(children, ", ") <> ")"

          {:coprod, 0} ->
            [child] = children
            "inl(#{child})"

          {:coprod, 1} ->
            [child] = children
            "inr(#{child})"

          {:coprod, n} ->
            [child] = children
            "in#{n}(#{child})"
        end
      end

      Term.cata(term, algebra)
    end

    @doc """
    Creates a generic term from a nested Elixir data structure.

    Supported inputs:
    - Non-negative integers → nat terms
    - Tuples → prod terms
    - `{:inl, term}` or `{:inr, term}` → coprod terms with index 0 or 1
    - `{:in, index, term}` → coprod term with given index

    ## Examples

        iex> from_elixir(42)
        nat(42)

        iex> from_elixir({1, 2, 3})
        prod([nat(1), nat(2), nat(3)])

        iex> from_elixir({:inl, 5})
        coprod(0, nat(5))
    """
    @spec from_elixir(term()) :: generic_term | no_return()
    def from_elixir(value) when is_integer(value) and value >= 0 do
      nat(value)
    end

    def from_elixir({:inl, inner}) do
      coprod(0, from_elixir(inner))
    end

    def from_elixir({:inr, inner}) do
      coprod(1, from_elixir(inner))
    end

    def from_elixir({:in, index, inner})
        when is_integer(index) and index >= 0 do
      coprod(index, from_elixir(inner))
    end

    def from_elixir(tuple) when is_tuple(tuple) do
      components =
        tuple
        |> Tuple.to_list()
        |> Enum.map(&from_elixir/1)

      prod(components)
    end

    def from_elixir(other) do
      raise ArgumentError, "Cannot convert #{inspect(other)} to generic term"
    end
  end

  defmodule FinSlicePolyF do
    alias Term

    @moduledoc """
    I am an enhanced representation of finitary polynomial functors on slice categories.

    Unlike `FinPolyF`, which specifies constructors only by their arity (number of
    parameters), I allow constructors to specify the *types* of their parameters.

    This allows for more expressive type systems where:
    1. Arity is described by a list of input types, not just an integer
    2. A language can define multiple, mutually recursive types
    3. Constructors for each type will specify parameter types, not just parameter count

    I can be viewed as a generalization of `FinPolyF` where we use slice categories
    instead of just the base category, allowing for dependent polynomial functors.
    The initial algebras of these functors are often called W-types.
    """

    @typedoc """
    A type index, identifying a specific type in a multi-type system.

    Type indices are non-negative integers, with each distinct type in a type
    system assigned a unique index.
    """
    @type type_index :: non_neg_integer()

    @typedoc """
    A constructor index, identifying a specific constructor for a given type.

    Constructor indices are non-negative integers, with each distinct constructor
    for a given type assigned a unique index.
    """
    @type ctor_index :: non_neg_integer()

    @typedoc """
    A type specification for a multi-type system.

    Consists of:
    - `input_types`: The number of input types in the system
    - `output_types`: The number of output types in the system
    - `ctor_counts`: A list specifying how many constructors each output type has
    - `ctor_types`: A function mapping (output_type_index, ctor_index) to a list of input type indices

    For initial algebras, input_types and output_types must be the same, but we allow
    them to differ to support composition of functors.
    """
    @type typespec :: %{
            input_types: non_neg_integer(),
            output_types: non_neg_integer(),
            ctor_counts: [non_neg_integer()],
            ctor_types: ({type_index, ctor_index} -> [type_index])
          }

    @typedoc """
    A constructor in the slice-based type system.

    Consists of:
    - `type_index`: The index of the output type this constructor produces
    - `ctor_index`: The index of this constructor within its output type
    """
    @type typed_ctor :: {type_index, ctor_index}

    @typedoc """
    The kinds of errors that can be produced by typecheck operations.

    * `{:invalid_constructor, ctor}` – the constructor is invalid
    * `{:invalid_type_index, type_index}` – the type index is out of bounds
    * `{:invalid_ctor_index, {type_index, ctor_index}}` – the constructor index is out of bounds for the given type
    * `{:invalid_param_type, {type_index, ctor_index}, param_index, type_index}` – a parameter has an incorrect type
    * `{:invalid_param_count, {type_index, ctor_index}, expected, actual}` – the number of children does not match the expected arity
    * `{:invalid_variable_type, v, expected, actual}` – a variable has an incorrect type
    """
    @type typecheck_error(ctor, v) ::
            {:invalid_constructor, ctor}
            | {:invalid_type_index, type_index}
            | {:invalid_ctor_index, {type_index, ctor_index}}
            | {:invalid_param_type, {type_index, ctor_index},
               non_neg_integer(), type_index}
            | {:invalid_param_count, {type_index, ctor_index},
               non_neg_integer(), non_neg_integer()}
            | {:invalid_variable_type, v, type_index, type_index}

    @typedoc """
    The result of typechecking.

    Returns:
      * `{:ok, type_index}` when the term passes the typecheck, including its output type
      * `{:error, errors}` when errors are produced, where errors is a list of `typecheck_error`
    """
    @type check_result(ctor, v) ::
            {:ok, type_index}
            | {:error, nonempty_list(typecheck_error(ctor, v))}

    @typedoc """
    Typecheck function for constructors.

    Given a constructor of type `ctor`, returns either:
    - `{:invalid_constructor}` (invalid constructor), or
    - `{:ok, typed_ctor}` (valid, with type and constructor indices)
    """
    @type tspec(ctor) :: (ctor -> {:invalid_constructor} | {:ok, typed_ctor})

    @typedoc """
    Typecheck function for variables.

    Given a variable of type `v`, returns either:
    - `{:ok, type_index}` (valid, with its type index), or
    - `{:invalid_variable}`
    """
    @type vspec(v) :: (v -> {:ok, type_index} | {:invalid_variable})

    @typedoc """
    A combined specification for open terms in a typed system.

    It is a triplet of:
      - a typespec defining the overall type system
      - a constructor specification (tspec)
      - a variable specification (vspec)
    """
    @type spec(ctor, v) :: {typespec, tspec(ctor), vspec(v)}

    @doc """
    Validates a typespec to ensure it's well-formed.

    A valid typespec must satisfy:
    1. input_types and output_types must be non-negative
    2. ctor_counts must have exactly output_types entries
    3. Each entry in ctor_counts must be non-negative
    4. ctor_types must return valid type indices for each valid constructor

    Returns :ok if valid, or {:error, reason} if invalid.
    """
    @spec validate_typespec(typespec) :: :ok | {:error, atom()}
    def validate_typespec(typespec) do
      %{
        input_types: _input_types,
        output_types: output_types,
        ctor_counts: ctor_counts,
        ctor_types: _ctor_types
      } = typespec

      cond do
        length(ctor_counts) != output_types ->
          {:error, :ctor_counts_length_mismatch}

        Enum.any?(ctor_counts, &(&1 < 0)) ->
          {:error, :negative_ctor_count}

        # Validate ctor_types for each valid type and constructor
        !validate_ctor_types(typespec) ->
          {:error, :invalid_ctor_types}

        true ->
          :ok
      end
    end

    @spec validate_ctor_types(typespec) :: boolean()
    defp validate_ctor_types(typespec) do
      %{
        input_types: input_types,
        output_types: output_types,
        ctor_counts: ctor_counts,
        ctor_types: ctor_types
      } = typespec

      # For each output type and each of its constructors, check if ctor_types returns valid type indices
      Enum.all?(0..(output_types - 1), fn type_idx ->
        ctor_count = Enum.at(ctor_counts, type_idx)

        Enum.all?(0..(ctor_count - 1), fn ctor_idx ->
          param_types = ctor_types.({type_idx, ctor_idx})

          # Check if all parameter types are valid
          Enum.all?(param_types, fn param_type ->
            param_type >= 0 && param_type < input_types
          end)
        end)
      end)
    end

    @doc """
    Typechecks an open term given a combined specification.

    The term is checked against the typespec using the constructor and variable
    specifications. For each constructor node, its type and constructor indices are verified,
    and the types of its children are checked against the expected parameter types.

    For variable nodes, their types are validated against the expected type.

    Returns:
      * `{:ok, type_index}` with the type of the term if validation passes
      * `{:error, errors}` with a list of all detected errors
    """
    @spec typecheck_v(
            Term.tv(ctor, {v, type_index}),
            spec(ctor, v)
          ) :: check_result(ctor, v)
          when ctor: term, v: term
    def typecheck_v(term, {typespec, tspec, vspec}) do
      %{
        input_types: _input_types,
        output_types: _output_types,
        ctor_counts: ctor_counts,
        ctor_types: ctor_types
      } = typespec

      Term.eval(
        # Constructor nodes
        fn {ctor, child_results} ->
          # Verify constructor validity and get its type info
          constructor_check =
            case tspec.(ctor) do
              {:invalid_constructor} ->
                {:error, [{:invalid_constructor, ctor}]}

              {:ok, {type_idx, ctor_idx}} ->
                # Verify type index is valid
                if type_idx < 0 || type_idx >= length(ctor_counts) do
                  {:error, [{:invalid_type_index, type_idx}]}
                else
                  # Verify constructor index is valid for this type
                  ctor_count = Enum.at(ctor_counts, type_idx)

                  if ctor_idx < 0 || ctor_idx >= ctor_count do
                    {:error, [{:invalid_ctor_index, {type_idx, ctor_idx}}]}
                  else
                    # Get expected parameter types for this constructor
                    expected_param_types = ctor_types.({type_idx, ctor_idx})
                    expected_param_count = length(expected_param_types)
                    actual_param_count = length(child_results)

                    # Check parameter count
                    if expected_param_count != actual_param_count do
                      {:error,
                       [
                         {:invalid_param_count, {type_idx, ctor_idx},
                          expected_param_count, actual_param_count}
                       ]}
                    else
                      # Now check each parameter's type
                      param_errors =
                        Enum.zip(
                          child_results,
                          Enum.with_index(expected_param_types)
                        )
                        |> Enum.flat_map(fn
                          {{:ok, actual_type}, {expected_type, param_idx}} ->
                            if actual_type != expected_type do
                              [
                                {:invalid_param_type, {type_idx, ctor_idx},
                                 param_idx, actual_type}
                              ]
                            else
                              []
                            end

                          {{:error, errs}, _} ->
                            errs
                        end)

                      if param_errors == [] do
                        {:ok, type_idx}
                      else
                        {:error, param_errors}
                      end
                    end
                  end
                end
            end

          constructor_check
        end,
        # Variable nodes
        fn {var, expected_type} ->
          case vspec.(var) do
            {:invalid_variable} ->
              {:error, [{:invalid_variable_type, var, expected_type, nil}]}

            {:ok, actual_type} ->
              if actual_type != expected_type do
                {:error,
                 [{:invalid_variable_type, var, expected_type, actual_type}]}
              else
                {:ok, expected_type}
              end
          end
        end,
        term
      )
    end

    @doc """
    A vspec that always returns a specific type index for any variable.

    Useful for simple cases where all variables are expected to have the same type.
    """
    @spec constant_vspec(type_index) :: vspec(term())
    def constant_vspec(type_idx) do
      fn _v -> {:ok, type_idx} end
    end

    @doc """
    Typechecks a closed term, using a default variable specification.

    Since the term is closed (no variables), the vspec is not actually used,
    but we provide one that would fail on any variable.
    """
    @spec typecheck(Term.t(ctor), typespec, tspec(ctor)) ::
            check_result(ctor, none())
          when ctor: term
    def typecheck(term, typespec, tspec) do
      # For closed terms, we use a vspec that would fail if ever called
      fail_vspec = fn _v -> {:invalid_variable} end
      typecheck_v(term, {typespec, tspec, fail_vspec})
    end

    @doc """
    Creates a simple typespec for a single type with specified constructors.

    This is a convenience function for creating a typespec when you have just one
    type and want to specify the arities of its constructors.

    Args:
      * `ctor_arities`: A list of arities for each constructor, e.g. [0, 2, 1]
        would create a type with three constructors having 0, 2, and 1 parameters respectively.

    The resulting type system has one type (index 0), and all parameters to all
    constructors are of this same type.
    """
    @spec simple_type(list(non_neg_integer())) :: typespec
    def simple_type(ctor_arities) do
      ctor_count = length(ctor_arities)

      %{
        input_types: 1,
        output_types: 1,
        ctor_counts: [ctor_count],
        ctor_types: fn {0, ctor_idx} ->
          arity = Enum.at(ctor_arities, ctor_idx)
          List.duplicate(0, arity)
        end
      }
    end

    @doc """
    Adapts a FinPolyF tspec to work with FinSlicePolyF.

    This allows code that uses the simpler FinPolyF constructor specification
    to work with the more complex FinSlicePolyF type system.

    Args:
      * `fin_tspec`: A tspec function from FinPolyF
      * `ctor_indices`: A map from constructors to their indices in the single type

    Returns a tspec function compatible with FinSlicePolyF where all constructors
    produce the same type (type index 0).
    """
    @spec adapt_fin_tspec(
            NockPoly.FinPolyF.tspec(ctor),
            %{optional(ctor) => ctor_index}
          ) :: tspec(ctor)
          when ctor: term
    def adapt_fin_tspec(fin_tspec, ctor_indices) do
      fn ctor ->
        case fin_tspec.(ctor) do
          {:invalid_constructor} ->
            {:invalid_constructor}

          {:ok, _arity} ->
            ctor_idx = Map.get(ctor_indices, ctor)
            {:ok, {0, ctor_idx}}
        end
      end
    end

    @doc """
    Creates a typespec from a list of type definitions.

    Each type definition is a list of constructor arities for that type.

    Args:
      * `type_defs`: A list of lists, where each inner list contains the arities
        for the constructors of a single type.
      * `param_types`: A function mapping {type_idx, ctor_idx, param_idx} to the
        type index of that parameter. This is required and must provide a mapping
        for every parameter.

    Example:
      ```
      # Define a typespec for simple binary trees:
      # Type 0: Tree = Leaf(value: Tree) | Node(left: Tree, right: Tree)
      typespec = create_typespec([[1, 2]], fn {_, _, _} -> 0 end)
      ```
    """
    @spec create_typespec(
            list(list(non_neg_integer())),
            ({type_index, ctor_index, non_neg_integer()} -> type_index)
          ) :: typespec
    def create_typespec(type_defs, param_types) do
      type_count = length(type_defs)
      ctor_counts = Enum.map(type_defs, &length/1)

      ctor_arities =
        Enum.with_index(type_defs)
        |> Enum.flat_map(fn {ctors, type_idx} ->
          Enum.with_index(ctors)
          |> Enum.map(fn {arity, ctor_idx} ->
            {{type_idx, ctor_idx}, arity}
          end)
        end)
        |> Map.new()

      %{
        input_types: type_count,
        output_types: type_count,
        ctor_counts: ctor_counts,
        ctor_types: fn {type_idx, ctor_idx} ->
          arity = Map.get(ctor_arities, {type_idx, ctor_idx}, 0)

          Enum.map(0..(arity - 1), fn param_idx ->
            param_types.({type_idx, ctor_idx, param_idx})
          end)
        end
      }
    end
  end

  defmodule FinIndIndPolyF do
    @moduledoc """
    I implement finitary inductive-inductive polynomial functors, which enable
    the definition of mutually dependent types where one depends on the constructors
    of the other.

    Inductive-inductive types consist of multiple type definitions where:
    1. The second type depends on the first type
    2. Each type can refer to both itself and the other type
    3. The second type can "see" the constructors of the first type

    This module extends the concepts from FinPolyF and FinSlicePolyF to support
    these more complex type dependencies.
    """

    alias Term

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
    @type representable_nt :: %{
            base_field_map: Term.fin_mapping(),
            dep_field_maps: [Term.fin_mapping()]
          }

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
    @type ind_ind_f1_nt :: %{
            pos_map: Term.fin_mapping(),
            rep_transformations: [representable_nt()]
          }

    @typedoc """
    I represent a slice over an IndIndF1, which is a pair of another IndIndF1
    and a natural transformation to the base IndIndF1.
    """
    @type ind_ind_f1_slice :: %{
            total: ind_ind_f1(),
            projection: ind_ind_f1_nt()
          }

    @typedoc """
    I represent a complete finitary inductive-inductive polynomial functor.

    I consist of a base IndIndF1 and a slice over it (the dependent part).
    """
    @type ind_ind_f :: %{
            base: ind_ind_f1(),
            slice: ind_ind_f1_slice()
          }

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
    def validate_representable_nt(rep_nt, source_rep, target_rep) do
      %{
        base_field_map: base_field_map,
        dep_field_maps: dep_field_maps
      } = rep_nt

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
    def validate_ind_ind_f1_nt(nt, source, target) do
      %{
        pos_map: pos_map,
        rep_transformations: rep_transformations
      } = nt

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
    def validate_ind_ind_f1_slice(slice, base) do
      %{
        total: total,
        projection: projection
      } = slice

      validate_ind_ind_f1_nt(projection, total, base)
    end

    @doc """
    I validate a complete IndIndF to ensure all its parts are well-formed.
    """
    @spec validate_ind_ind_f(ind_ind_f()) :: :ok | {:error, atom()}
    def validate_ind_ind_f(f) do
      %{
        base: base,
        slice: slice
      } = f

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

  defmodule Fin2ForestPolyF do
    @moduledoc """
    I implement finitary polynomial functors over finite two-level forests.

    A finite two-level forest represents a two-level type system where:
    - Base types form the first level
    - Each base type can have dependent types that form the second level
    - Each dependent type has exactly one parent base type

    This generalizes both:
    - FinSlicePolyF: Multiple types with typed parameters
    - FinIndIndPolyF: Inductive-inductive types with dependencies

    The forest is represented as a list of natural numbers where:
    - Length = number of base types
    - Each element = number of dependent types for that base type

    Example: [2, 0, 3] represents:
    - 3 base types (indexed 0, 1, 2)
    - Base type 0 has 2 dependent types
    - Base type 1 has 0 dependent types
    - Base type 2 has 3 dependent types

    ## Relation to Parametric Right Adjoints (PRA)

    This module implements PRA functors as described in:
    https://ncatlab.org/nlab/show/parametric+right+adjoint#generic_morphisms

    Our finite two-level forests represent index categories where morphisms
    only exist from dependent objects to their parent base objects (plus identities).
    The polynomial functors we define here are instances of PRA functors between
    presheaf categories. When the forest is [1] (one base with one dependent),
    we get the walking arrow category, the simplest non-trivial PRA functor case.
    """

    alias Term

    @typedoc """
    A finite two-level forest specification.

    A list where each element specifies how many dependent types depend on
    that base type.

    In PRA theory, this represents the structure of an index category. For a
    PRA functor [C^op, Set] → [D^op, Set], both C and D would be represented
    as fin2_forest structures. The forest determines the objects and morphisms
    of the index category. A type dependency is expressed as a morphism from
    the dependent type to the base type, which is a standard category-theoretic
    use of a morphism as a fibration.
    """
    @type fin2_forest :: [non_neg_integer()]

    @typedoc """
    An object in the forest is either a base type or a dependent type.

    - `{:base, index}` - A base type with the given index
    - `{:dep, base_index, dep_index}` - A dependent type of the given base

    These correspond to objects in the index category. In the context of
    PRA functors and the nLab article's el(T1), these are the objects that
    copresheaves assign sets (or types) to. The morphisms in this category
    are identities plus unique morphisms from each dependent object to its base.
    """
    @type forest_obj ::
            {:base, non_neg_integer()}
            | {:dep, non_neg_integer(), non_neg_integer()}

    @typedoc """
    A position (constructor) representation in the forest.

    For each position, we specify:
    - Which forest objects are parameters (list of forest_obj)
    - The parameter list determines both arity and types

    In PRA functor theory, this corresponds to a position in a polynomial
    functor P(X) = Σ_{p ∈ Pos} X^{Dir(p)}. Each position_spec defines:
    - The position p (implicitly, by its location in the position_map)
    - The direction type Dir(p) via the list of parameter types
    - The arity |Dir(p)| is the length of the parameter list

    For example, [{:base, 0}, {:dep, 0, 1}] represents a position whose
    direction type requires two inputs: one from base type 0 and one from
    dependent type 1 of base 0.
    """
    @type position_spec :: [forest_obj()]

    @typedoc """
    A position map assigns position specs to each constructor.

    This is a map from position indices to their specifications.

    In PRA functor theory, this represents the position type (Pos) of a
    polynomial functor. The map structure gives us:
    - A finite set of positions (the keys 0, 1, 2, ...)
    - For each position, its direction type specification

    When building polynomial functors over our index category, each type
    (base or dependent) has its own position map that defines the constructors
    for building elements of that type.

    Example: %{0 => [], 1 => [{:base, 0}]} defines a polynomial with two
    positions: position 0 has no parameters (constant), and position 1 has
    one parameter from base type 0.

    ## Role in Polynomial Functors

    In the polynomial formula P(X) = Σ_{p ∈ Pos} X^{Dir(p)}:
    - The map keys form the position set Pos
    - Each value (position_spec) defines Dir(p), the "direction type" or "arity"
    - X^{Dir(p)} means we need |Dir(p)| inputs of the appropriate types

    This realizes the "generic morphism" aspect of PRA functors: each position
    represents a way to construct an element, and the position spec tells us
    what inputs we need. The polynomial functor sums over all possible ways
    (positions) to construct elements.
    """
    @type position_map :: %{non_neg_integer() => position_spec()}

    @typedoc """
    A polynomial functor specification over a finite two-level forest.

    This specifies a PRA (parametric right adjoint) endofunctor on the copresheaf
    category determined by the forest structure. In the context of the nLab article
    on PRA functors, this represents:

    - The polynomial functor P(X) = Σ_{p ∈ Pos} X^{Dir(p)}
    - Where Pos is the disjoint union of all position sets (one per type)
    - And Dir(p) specifies the parameter types for position p

    The specification consists of:
    - `forest`: The forest structure defining the index category
    - `base_positions`: Position maps for base type constructors
    - `dep_positions`: Position maps for dependent type constructors

    Together, these define how to build new elements of each type from existing
    elements, which is precisely what a polynomial endofunctor does.

    ## Relation to PRA Theory

    In parametric right adjoint theory, a functor T : [C^op, Set] → [D^op, Set]
    is uniquely determined by:
    1. An object T1 ∈ [D^op, Set] (a copresheaf on the target category)
    2. A functor E_T : el(T1)^op → [C^op, Set] from the opposite of T1's category
       of elements

    The action is given by: T(Z)(j) = Σ_{i ∈ T1(j)} Hom[C^op, Set](E_T(j,i), Z)

    In our polynomial functor representation:
    - The position maps collectively define T1: for each object j in the forest,
      T1(j) is the set of positions (constructors) available at that type
    - The parameter specifications in each position define E_T: for each position
      p at object j, E_T(j,p) specifies what inputs are needed
    - The polynomial structure P(X) = Σ_{p ∈ Pos} X^{Dir(p)} directly implements
      the PRA formula above
    """
    @type forest_poly_spec :: %{
            forest: fin2_forest(),
            base_positions: [position_map()],
            dep_positions: [[position_map()]]
          }

    @doc """
    Get the number of base types in a forest.
    """
    @spec num_base_types(fin2_forest()) :: non_neg_integer()
    def num_base_types(forest), do: length(forest)

    @doc """
    Get the number of dependent types for a specific base type.
    """
    @spec num_dep_types(fin2_forest(), non_neg_integer()) :: non_neg_integer()
    def num_dep_types(forest, base_idx) when base_idx >= 0 do
      Enum.at(forest, base_idx, 0)
    end

    @doc """
    Validate that a forest object is valid for the given forest.
    """
    @spec validate_forest_obj(forest_obj(), fin2_forest()) ::
            :ok | {:error, atom()}
    def validate_forest_obj({:base, idx}, forest) do
      if idx >= 0 and idx < num_base_types(forest) do
        :ok
      else
        {:error, :invalid_base_index}
      end
    end

    def validate_forest_obj({:dep, base_idx, dep_idx}, forest) do
      cond do
        base_idx < 0 or base_idx >= num_base_types(forest) ->
          {:error, :invalid_base_index}

        dep_idx < 0 or dep_idx >= num_dep_types(forest, base_idx) ->
          {:error, :invalid_dep_index}

        true ->
          :ok
      end
    end

    @doc """
    Validate a forest specification.
    """
    @spec validate_forest_spec(forest_poly_spec()) :: :ok | {:error, atom()}
    def validate_forest_spec(spec) do
      %{
        forest: forest,
        base_positions: base_positions,
        dep_positions: dep_positions
      } = spec

      num_bases = num_base_types(forest)

      cond do
        length(base_positions) != num_bases ->
          {:error, :base_positions_length_mismatch}

        length(dep_positions) != num_bases ->
          {:error, :dep_positions_length_mismatch}

        !validate_all_positions(spec) ->
          {:error, :invalid_position_specs}

        true ->
          :ok
      end
    end

    # Validate all position specifications
    @spec validate_all_positions(forest_poly_spec()) :: boolean()
    defp validate_all_positions(spec) do
      %{
        forest: forest,
        base_positions: base_positions,
        dep_positions: dep_positions
      } = spec

      # Check all base positions
      base_valid =
        Enum.with_index(base_positions)
        |> Enum.all?(fn {pos_map, _base_idx} ->
          Enum.all?(pos_map, fn {_pos_idx, param_list} ->
            Enum.all?(param_list, &(validate_forest_obj(&1, forest) == :ok))
          end)
        end)

      dep_valid =
        Enum.with_index(dep_positions)
        |> Enum.all?(fn {dep_list, base_idx} ->
          expected_deps = num_dep_types(forest, base_idx)

          if length(dep_list) != expected_deps do
            false
          else
            Enum.all?(dep_list, fn pos_map ->
              Enum.all?(pos_map, fn {_pos_idx, param_list} ->
                Enum.all?(
                  param_list,
                  &(validate_forest_obj(&1, forest) == :ok)
                )
              end)
            end)
          end
        end)

      base_valid and dep_valid
    end

    @typedoc """
    A constructor tag for generic terms in the forest polynomial functor.

    These are the labels that appear at nodes of `Term.t(forest_ctor())` and
    `Term.tv(forest_ctor(), v)`. Each constructor specifies:
    - Which type it constructs (base or dependent)
    - Which specific constructor it is for that type

    Formats:
    - `{:base, type_idx, ctor_idx}` - The ctor_idx'th constructor of base type type_idx
    - `{:dep, base_idx, dep_idx, ctor_idx}` - The ctor_idx'th constructor of the
      dep_idx'th dependent type of base type base_idx

    These constructor tags, combined with a forest_poly_spec, determine the arity
    and parameter types expected for each node in a term.
    """
    @type forest_ctor ::
            {:base, non_neg_integer(), non_neg_integer()}
            | {:dep, non_neg_integer(), non_neg_integer(), non_neg_integer()}

    @typedoc """
    Typecheck errors specific to forest types.
    """
    @type typecheck_error(v) ::
            {:invalid_constructor, forest_ctor()}
            | {:invalid_arity, forest_ctor(), non_neg_integer(),
               non_neg_integer()}
            | {:invalid_param_type, forest_ctor(), non_neg_integer(),
               forest_obj()}
            | {:invalid_variable, v}

    @doc """
    Get the expected parameters for a constructor.

    Returns {:ok, [forest_obj]} or {:error, reason}.
    """
    @spec get_ctor_params(forest_poly_spec(), forest_ctor()) ::
            {:ok, [forest_obj()]} | {:error, atom()}
    def get_ctor_params(spec, {:base, type_idx, ctor_idx}) do
      case Enum.at(spec.base_positions, type_idx) do
        nil ->
          {:error, :invalid_type_index}

        pos_map ->
          case Map.get(pos_map, ctor_idx) do
            nil -> {:error, :invalid_ctor_index}
            params -> {:ok, params}
          end
      end
    end

    def get_ctor_params(spec, {:dep, base_idx, dep_idx, ctor_idx}) do
      case Enum.at(spec.dep_positions, base_idx) do
        nil ->
          {:error, :invalid_base_index}

        dep_list ->
          case Enum.at(dep_list, dep_idx) do
            nil ->
              {:error, :invalid_dep_index}

            pos_map ->
              case Map.get(pos_map, ctor_idx) do
                nil -> {:error, :invalid_ctor_index}
                params -> {:ok, params}
              end
          end
      end
    end

    @doc """
    Get the type of a constructor (which forest object it constructs).
    """
    @spec get_ctor_type(forest_ctor()) :: forest_obj()
    def get_ctor_type({:base, type_idx, _ctor_idx}) do
      {:base, type_idx}
    end

    def get_ctor_type({:dep, base_idx, dep_idx, _ctor_idx}) do
      {:dep, base_idx, dep_idx}
    end

    @typedoc """
    An algebra for eliminating forest terms.

    This is a standard term algebra extended with the forest polynomial specification
    as additional context. Where a regular term algebra receives (constructor, child_results),
    a forest algebra receives (constructor, child_results, spec).

    The spec parameter provides access to the polynomial functor specification,
    allowing the algebra to:
    - Look up constructor arities and parameter types
    - Access the forest structure
    - Make decisions based on the type system context

    Type signature: (forest_ctor(), [r], forest_poly_spec()) -> r

    ## Computational Interpretation

    A forest algebra defines how to compute a result of type `r` from a term tree:
    - Each constructor (position) represents a computation pattern
    - The child_results are the already-computed results from subterms
    - The algebra combines these results according to the constructor's meaning

    For example, an algebra evaluating arithmetic expressions might:
    - Map {:base, 0, 0} (a "zero" constructor) to the number 0
    - Map {:base, 0, 1} (a "successor" constructor) to child_result + 1
    - Map {:base, 0, 2} (an "add" constructor) to sum(child_results)

    The spec parameter allows the algebra to be generic over different polynomial
    functors, making decisions based on the structure of the type system rather
    than hard-coding specific constructors.
    """
    @type forest_algebra(r) :: (forest_ctor(), [r], forest_poly_spec() -> r)

    @doc """
    Evaluate a forest term with a substitution function for variables.

    This is the general elimination principle for open forest terms (terms that
    may contain variables). It allows pattern matching and computation while
    respecting the type structure.

    Parameters:
    - `term`: The forest term to evaluate
    - `algebra`: Function that handles each constructor
    - `var_handler`: Function that handles variables (substitution)
    - `spec`: The forest specification (passed to algebra for context)

    Returns the result of evaluating the term with the algebra.
    """
    @spec eval(
            Term.tv(forest_ctor(), v),
            forest_algebra(r),
            (v -> r),
            forest_poly_spec()
          ) :: r
          when v: term, r: term
    def eval(term, algebra, var_handler, spec) do
      Term.eval(
        fn {ctor, child_results} ->
          algebra.(ctor, child_results, spec)
        end,
        var_handler,
        term
      )
    end

    @doc """
    Catamorphism (fold) for closed forest terms.

    This is the elimination principle for closed forest terms (terms with no
    variables). It's a special case of eval where variables cause an error.

    Parameters:
    - `term`: The closed forest term to fold
    - `algebra`: Function that handles each constructor
    - `spec`: The forest specification (passed to algebra for context)

    Returns the result of folding the term with the algebra.
    """
    @spec cata(
            Term.t(forest_ctor()),
            forest_algebra(r),
            forest_poly_spec()
          ) :: r
          when r: term
    def cata(term, algebra, spec) do
      Term.cata(
        term,
        fn {ctor, child_results} ->
          algebra.(ctor, child_results, spec)
        end
      )
    end

    @doc """
    Typecheck a term against a forest specification.

    Returns {:ok, forest_obj} with the type of the term, or {:error, errors}.
    """
    @spec typecheck(Term.tv(forest_ctor(), v), forest_poly_spec()) ::
            {:ok, forest_obj()} | {:error, [typecheck_error(v)]}
          when v: term
    def typecheck(term, spec) do
      Term.eval(
        # Constructor case
        fn {ctor, child_results} ->
          case get_ctor_params(spec, ctor) do
            {:error, _reason} ->
              {:error, [{:invalid_constructor, ctor}]}

            {:ok, expected_params} ->
              expected_arity = length(expected_params)
              actual_arity = length(child_results)

              if expected_arity != actual_arity do
                {:error,
                 [{:invalid_arity, ctor, expected_arity, actual_arity}]}
              else
                # Check parameter types
                param_errors =
                  Enum.zip(child_results, Enum.with_index(expected_params))
                  |> Enum.flat_map(fn
                    {{:ok, actual_type}, {expected_type, idx}} ->
                      if actual_type == expected_type do
                        []
                      else
                        [{:invalid_param_type, ctor, idx, actual_type}]
                      end

                    {{:error, errs}, _} ->
                      errs
                  end)

                if param_errors == [] do
                  {:ok, get_ctor_type(ctor)}
                else
                  {:error, param_errors}
                end
              end
          end
        end,
        # Variable case
        fn var ->
          {:error, [{:invalid_variable, var}]}
        end,
        term
      )
    end

    @doc """
    Create a simple forest specification from constructor arities.

    This is a convenience function for simple cases where:
    - There's only one base type
    - All parameters are of the base type
    - No dependent types

    Example: simple_forest_spec([2, 0, 1]) creates a single base type with
    three constructors having 2, 0, and 1 parameters respectively.
    """
    @spec simple_forest_spec([non_neg_integer()]) :: forest_poly_spec()
    def simple_forest_spec(ctor_arities) do
      base_positions =
        ctor_arities
        |> Enum.with_index()
        |> Enum.map(fn {arity, idx} ->
          {idx, List.duplicate({:base, 0}, arity)}
        end)
        |> Map.new()

      %{
        # One base type, no dependents
        forest: [0],
        base_positions: [base_positions],
        dep_positions: [[]]
      }
    end

    @doc """
    Create a polynomial functor specification from constructor specifications.

    This constructs a PRA endofunctor on the copresheaf category of the given forest.
    In categorical terms, we're defining:
    - A polynomial functor P: [C^op, Set] → [C^op, Set]
    - Where C is the index category determined by the forest
    - The functor maps each copresheaf X to P(X) = Σ_{p ∈ Pos} X^{Dir(p)}

    The position types (Pos) are partitioned by object in the index category,
    and the direction function Dir assigns to each position its list of parameter types.

    Args:
    - forest: The forest structure defining the index category
    - base_ctor_specs: For each base type, a list of parameter specifications
    - dep_ctor_specs: For each base type, dependent constructor specifications

    Example:
    ```
    create_forest_spec(
      [2, 0],  # Base 0 has 2 deps, base 1 has 0 deps
      [        # Base constructors
        [[{:base, 0}, {:base, 1}]],  # Base 0, ctor 0: takes two base params
        [[]]                          # Base 1, ctor 0: takes no params
      ],
      [        # Dependent constructors for each base
        [      # Base 0's dependents
          [[{:dep, 0, 0}]],           # Dep 0, ctor 0: takes one param
          [[{:base, 0}], []]          # Dep 1, ctor 0 and 1
        ],
        []     # Base 1 has no deps
      ]
    )
    ```

    This specification defines the data needed to:
    1. Construct terms (via positions and their parameter types)
    2. Type-check terms (via expected parameter types)
    3. Eliminate terms (via pattern matching with algebras)
    """
    @spec create_forest_spec(
            fin2_forest(),
            [[position_spec()]],
            [[[position_spec()]]]
          ) :: forest_poly_spec()
    def create_forest_spec(forest, base_ctor_specs, dep_ctor_specs) do
      # Convert lists to position maps
      to_pos_map = fn ctor_list ->
        ctor_list
        |> Enum.with_index()
        |> Enum.map(fn {params, idx} -> {idx, params} end)
        |> Map.new()
      end

      base_positions = Enum.map(base_ctor_specs, to_pos_map)

      dep_positions =
        Enum.map(dep_ctor_specs, fn dep_list ->
          Enum.map(dep_list, to_pos_map)
        end)

      %{
        forest: forest,
        base_positions: base_positions,
        dep_positions: dep_positions
      }
    end
  end

  defmodule NockTerms do
    @moduledoc """
    I am a polynomial specification for Nock terms.

    A Nock term is a binary tree where:
      - An atom node is represented as `{:atom, noun}` (with `noun` having
        type Noun.noun_atom()).  Atoms have arity 0.
      - A cell node is represented as `:cell`.
        Cells have arity 2.

    All constructors are always valid (any term of type `Noun.noun_atom()` is
    a valid Nock atom).
    """

    alias Noun
    alias NockPoly.FinPolyF

    @type nock_term_ctor :: {:atom, Noun.noun_atom()} | :cell
    @type nock_poly_term :: Term.t(nock_term_ctor)

    @spec nock_tspec(nock_term_ctor) :: {:ok, non_neg_integer()}
    def nock_tspec({:atom, _noun}) do
      {:ok, 0}
    end

    def nock_tspec(:cell) do
      {:ok, 2}
    end

    @spec typecheck(nock_poly_term) ::
            FinPolyF.check_result(nock_term_ctor, none())
    def typecheck(term) do
      FinPolyF.typecheck(term, &nock_tspec/1)
    end

    @doc """
    I convert a Noun.t() into a Nock polynomial term (nock_poly_term).

    If `noun` satisfies Noun.is_noun_atom/1, it is wrapped as an atom;
    otherwise it must be a cell represented as a two-element list, which is
    recursively converted. This function always succeeds and produces a term
    that passes the Nock typecheck.
    """
    @spec from_noun(Noun.t()) :: nock_poly_term
    def from_noun(noun) do
      cond do
        Noun.is_noun_atom(noun) ->
          Term.com_tv({:atom, noun}, [])

        Noun.is_noun_cell(noun) ->
          case noun do
            [left | right] ->
              Term.com_tv(:cell, [from_noun(left), from_noun(right)])
          end
      end
    end

    @spec to_noun_algebra({nock_term_ctor, [Noun.t()]}) :: Noun.t()
    defp to_noun_algebra({ctor, results}) do
      case {ctor, results} do
        {{:atom, noun}, []} ->
          noun

        {:cell, [left, right]} ->
          [left | right]
      end
    end

    @doc """
    I convert a Nock polynomial term (nock_poly_term) back into a Noun.t().

    It is assumed that the term has successfully passed the Nock typecheck.
    I use `cata/2` (the catamorphism) so that I am not directly recursive.
    """
    @spec to_noun(nock_poly_term) :: Noun.t()
    def to_noun(term) do
      Term.cata(term, &to_noun_algebra/1)
    end

    @typedoc "I am an open Nock polynomial term containing metavariables of type `v`."
    @type open_nock_poly_term(v) :: Term.tv(nock_term_ctor, v)

    @doc """
    I substitute metavariables in an open Nock term using the provided
    substitution function.

    Given an open Nock term of type `open_nock_poly_term(v)` and a substitution
    function `f` from `v` to a closed Nock term, I return a closed
    Nock term (of type `nock_poly_term`).

    I am implemented using monadic bind (`tv_bind`).
    """
    @spec substitute(open_nock_poly_term(v), (v -> nock_poly_term)) ::
            nock_poly_term
          when v: term
    def substitute(term, f) do
      Term.tv_bind(f, term)
    end
  end
end
