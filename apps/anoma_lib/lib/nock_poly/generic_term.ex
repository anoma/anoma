defmodule NockPoly.GenericTerm do
  @moduledoc """
  I provide a generic term representation for typechecking against unified
  polynomial functors.

  I support three kinds of constructors:
  - `:coprod` - Represents a coproduct (sum type) with an index and a term
  - `:prod` - Represents a product type with a list of component terms
  - `:nat` - Represents a natural number literal

  This representation allows building nested structures without
  having to encode everything arithmetically as natural numbers. Terms
  of this type can be typechecked against both slice polynomial functors
  and inductive-inductive types.
  """

  alias NockPoly.Term
  alias NockPoly.FinPolyF

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
          | :invalid_constructor
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
    :invalid_constructor
  end

  @doc """
  Typechecks a generic term to ensure it's well-formed.

  This only checks the structure of the term itself, not whether it
  conforms to any particular type specification.
  """
  @spec typecheck(generic_term) ::
          FinPolyF.check_result(generic_ctor, none())
  def typecheck(term) do
    Term.cata(term, &typecheck_algebra/1)
  end

  @spec typecheck_algebra(
          {generic_ctor, [FinPolyF.check_result(generic_ctor, none())]}
        ) ::
          FinPolyF.check_result(generic_ctor, none())
  defp typecheck_algebra({ctor, child_results}) do
    child_errors =
      child_results
      |> Enum.flat_map(fn
        :ok -> []
        {:error, errs} -> errs
      end)

    level_result =
      case {ctor, length(child_results)} do
        {{:coprod, _index}, 1} ->
          :ok

        {{:coprod, index}, count} ->
          {:error, [{:invalid_arity, {:coprod, index}, 1, count}]}

        {:prod, _count} ->
          :ok

        {{:nat, _value}, 0} ->
          :ok

        {{:nat, value}, count} ->
          {:error, [{:invalid_arity, {:nat, value}, 0, count}]}

        {ctor, _} ->
          {:error, [{:invalid_constructor, ctor}]}
      end

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
