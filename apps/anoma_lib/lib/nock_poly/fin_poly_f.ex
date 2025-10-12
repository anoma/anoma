defmodule NockPoly.FinPolyF do
  @moduledoc """
  I am the simplest form that we define of finitary polynomial functor.

  This specification is parameterized on an error type `e` (interpreted as
  the type of possible typecheck errors) and on a constructor type `ctor`. It
  is represented as a function from a constructor value to an either type,
  where the `:error` variant carries an error of type `e` and the `:ok` variant
  carries a natural number (representing the arity).
  """

  alias NockPoly.Term

  @typedoc "Typecheck function for constructors. Given a constructor of type `ctor`, returns either `{:error}` (invalid) or `{:ok, non_neg_integer()}` (valid, with the returned arity)."
  @type tspec(ctor) :: (ctor ->
                          :invalid_constructor
                          | {:ok, non_neg_integer()})

  @typedoc "Typecheck function for variables. Given a variable of type `v`, returns either `:ok` (valid) or `{:error, [:invalid_variable]}`."
  @type vspec(v) :: (v -> {:ok} | :invalid_variable)

  @doc "A `vspec` which always succeeds (returning `:ok`)."
  @spec vspec_ok(v) :: :ok when v: term
  def vspec_ok(_v) do
    :ok
  end

  @typedoc """
  A combined specification for open terms.
  It is a pair of:
    - a constructor specification (tspec), and
    - a variable specification (vspec).
  """
  @type spec(ctor, v) :: {tspec(ctor), vspec(v)}

  @typedoc """
  The kinds of errors that can be produced by typecheck/2.

  * `{:invalid_constructor, ctor}` – the constructor is invalid.
  * `{:invalid_arity, ctor, expected, actual}` – the number of children does not match the expected arity.
  * `:invalid_variable` – the variable failed its check.
  """
  @type typecheck_error(ctor, v) ::
          {:invalid_constructor, ctor}
          | {:invalid_arity, ctor, non_neg_integer(), non_neg_integer()}
          | {:invalid_variable, v}

  @typedoc """
  The result of typechecking.

  It returns:
    * `:ok` when the term passes the typecheck.
    * `{:error, errors}` when errors are produced, where errors is a list of `typecheck_error`.
  """
  @type check_result(ctor, v) ::
          :ok | {:error, nonempty_list(typecheck_error(ctor, v))}

  @doc """
  Typechecks an open term given a combined specification.

  The combined specification is a pair `{tspec, vspec}`, where:
    • `tspec` is a function from a constructor to either
        `{:error, :invalid_constructor}` (invalid constructor)
      or `{:ok, non_neg_integer()}` (valid with the expected arity).
      If the number of children does not equal the expected arity, an error
      `{:invalid_arity, ctor, expected, actual}` is produced.
    • `vspec` is a function which typechecks variable nodes, returning either
      `:ok` or `{:error, :invalid_variable}`.

  For a constructor node, errors detected at this level are concatenated with errors
  from the children. If no errors occur, `:ok` is returned; otherwise, `{:error, errors}`.
  """

  @spec typecheck_v(
          Term.tv(ctor, v),
          {tspec(ctor), vspec(v)}
        ) :: check_result(ctor, v)
        when ctor: term, v: term
  def typecheck_v(term, {tspec, vspec}) do
    Term.eval(
      fn {ctor, child_results} ->
        children_count = length(child_results)

        level_errors =
          case tspec.(ctor) do
            :invalid_constructor ->
              [{:invalid_constructor, ctor}]

            {:ok, expected} ->
              if children_count != expected do
                [{:invalid_arity, ctor, expected, children_count}]
              else
                []
              end
          end

        children_errors =
          child_results
          |> Enum.flat_map(fn
            :ok -> []
            {:error, errs} -> errs
          end)

        errors = level_errors ++ children_errors

        if errors == [] do
          :ok
        else
          {:error, errors}
        end
      end,
      # Wrap the variable checker to convert any error to a singleton list.
      fn var ->
        case vspec.(var) do
          :ok -> :ok
          :invalid_variable -> {:error, [{:invalid_variable, var}]}
        end
      end,
      term
    )
  end

  @doc """
  I typecheck a *closed* term by wrapping `typecheck_v/2` with a vspec
  that always returns :ok.
  """
  @spec typecheck(Term.t(ctor), tspec(ctor)) ::
          check_result(ctor, none())
        when ctor: term
  def typecheck(term, tspec) do
    typecheck_v(term, {tspec, &vspec_ok/1})
  end
end
