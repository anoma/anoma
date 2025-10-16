defmodule NockPoly.Fin2ForestPolyF do
  use TypedStruct

  alias NockPoly.Term

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
  we get the walking arrow category.
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

  typedstruct module: ForestPolySpec, enforce: true do
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

    field(:forest, NockPoly.Fin2ForestPolyF.fin2_forest())
    field(:base_positions, [NockPoly.Fin2ForestPolyF.position_map()])
    field(:dep_positions, [[NockPoly.Fin2ForestPolyF.position_map()]])
  end

  @type forest_poly_spec :: ForestPolySpec.t()

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
  def validate_forest_spec(
        %{
          forest: forest,
          base_positions: base_positions,
          dep_positions: dep_positions
        } = spec
      ) do
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
  defp validate_all_positions(%{
         forest: forest,
         base_positions: base_positions,
         dep_positions: dep_positions
       }) do
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
              {:error, [{:invalid_arity, ctor, expected_arity, actual_arity}]}
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

    %ForestPolySpec{
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

    %ForestPolySpec{
      forest: forest,
      base_positions: base_positions,
      dep_positions: dep_positions
    }
  end
end
