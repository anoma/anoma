defmodule NockPoly.FinSlicePolyF do
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

  alias NockPoly.Term

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
          | {:invalid_param_type, {type_index, ctor_index}, non_neg_integer(),
             type_index}
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
  - `:invalid_constructor` (invalid constructor), or
  - `{:ok, typed_ctor}` (valid, with type and constructor indices)
  """
  @type tspec(ctor) :: (ctor -> :invalid_constructor | {:ok, typed_ctor})

  @typedoc """
  Typecheck function for variables.

  Given a variable of type `v`, returns either:
  - `{:ok, type_index}` (valid, with its type index), or
  - `:invalid_variable`
  """
  @type vspec(v) :: (v -> {:ok, type_index} | :invalid_variable)

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
  def validate_typespec(
        %{
          output_types: output_types,
          ctor_counts: ctor_counts
        } = typespec
      ) do
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
  defp validate_ctor_types(%{
         input_types: input_types,
         output_types: output_types,
         ctor_counts: ctor_counts,
         ctor_types: ctor_types
       }) do
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
  def typecheck_v(
        term,
        {%{ctor_counts: ctor_counts, ctor_types: ctor_types}, tspec, vspec}
      ) do
    Term.eval(
      # Constructor nodes
      fn {ctor, child_results} ->
        # Verify constructor validity and get its type info
        constructor_check =
          case tspec.(ctor) do
            :invalid_constructor ->
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
          :invalid_variable ->
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
    fail_vspec = fn _v -> :invalid_variable end
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
        :invalid_constructor ->
          :invalid_constructor

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
