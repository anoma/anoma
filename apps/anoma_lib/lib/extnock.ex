defmodule ExtNock do
  @moduledoc """
  Macros for writing open polynomial Nock terms as an internal DSL.

  These macros compile our extended (open) Nock term language
  into the underlying representation provided by `NockPoly`.

  In the future, these macros will support metavariables and substitution,
  allowing library definitions to be written in a DSL that resembles Hoon.
  """

  require Noun
  require NockPoly

  # I contain `defmacro`s (and `defmacros` only), to distinguish them
  # for ignoring code coverage (which does not know how to tell whether
  # a macro has been expanded and executed).
  defmodule MacroDefs do
  end

  defmodule ExtNockTerms do
    @moduledoc """
    I provide extended Nock term functionality, adding semantic constructors
    beyond the basic `:atom` and `:cell` constructors of standard Nock.

    Extended terms comprise an internal supserset of Nock whch can be compiled
    down to standard Nock formulas for execution.
    """
    alias NockPoly.Term
    alias NockPoly.NockTerms

    @typedoc """
    I represent an extended constructor for Nock terms, providing higher-level
    semantic operations beyond raw Nock.

    As development progresses, I will be expanded to include more constructors
    for all Nock operations and library functions.
    """
    @type ext_term_ctor :: :slot | :constant | :evaluate

    @typedoc """
    I am a constructor for extended Nock terms, which can be either a standard
    Nock term constructor or an extended constructor.
    """
    @type ext_nock_term_ctor :: NockTerms.nock_term_ctor() | ext_term_ctor()

    @typedoc """
    I am an extended polynomial Nock term with variables of type `v`.
    I extend the standard `open_nock_poly_term` with additional constructors.
    """
    @type ext_nock_poly_term(v) :: Term.tv(ext_nock_term_ctor(), v)

    @doc """
    I provide a type specification for extended constructors, defining
    their expected arities.

    I return {:ok, arity} for valid constructors with their expected arity,
    or {:standard, ctor} for (what I expect to be) a standard Nock constructor.
    """
    @spec ext_tspec(ext_term_ctor()) ::
            {:ok, non_neg_integer()} | {:standard, NockTerms.nock_term_ctor()}
    def ext_tspec(:slot), do: {:ok, 1}
    def ext_tspec(:constant), do: {:ok, 1}
    def ext_tspec(:evaluate), do: {:ok, 2}
    def ext_tspec(ctor), do: {:standard, ctor}

    @doc """
    I provide a type specification function for all term constructors
    (extended constructors or standard Nock constructors).

    I first check if the constructor is an extended constructor, and if not,
    I delegate to the standard Nock term typespec function.
    """
    @spec ext_nock_poly_tspec(ext_nock_term_ctor()) ::
            {:ok, non_neg_integer()} | {:invalid_constructor}
    def ext_nock_poly_tspec(ctor) do
      case ext_tspec(ctor) do
        {:ok, arity} ->
          {:ok, arity}

        {:standard, sctor} ->
          NockTerms.nock_tspec(sctor)
      end
    end

    @doc """
    I typecheck an extended Nock polynomial term.

    I use the FinPolyF typecheck system with our extended constructor typespec
    function to validate the entire term tree.
    """
    @spec typecheck(ext_nock_poly_term(v)) ::
            NockPoly.FinPolyF.check_result(ext_nock_term_ctor(), v)
          when v: term()
    def typecheck(term) do
      NockPoly.FinPolyF.typecheck_v(
        term,
        {&ext_nock_poly_tspec/1, &NockPoly.FinPolyF.vspec_ok/1}
      )
    end

    @type ext_nock_compile_error_list(v) ::
            [
              NockPoly.FinPolyF.typecheck_error(ext_nock_term_ctor(), v)
            ]

    @type compile_result(v) ::
            {:ok, NockTerms.open_nock_poly_term(v)}
            | {:error, ext_nock_compile_error_list(v)}

    @doc """
    I implement the algebra for compiling extended term constructors to standard
    Nock term constructors.

    I should only be called with terms that have already been typechecked,
    as I assume that each constructor has the correct arity.

    I handle each extended constructor by transforming it into an equivalent
    representation using only standard Nock constructors.
    """
    @spec compile_algebra() ::
            (Term.termf(
               ext_nock_term_ctor(),
               NockTerms.open_nock_poly_term(v)
             ) ->
               NockTerms.open_nock_poly_term(v))
          when v: term()
    def compile_algebra do
      fn
        {:slot, [addr]} ->
          {:cell, [{{:atom, 0}, []}, addr]}

        {:constant, [val]} ->
          {:cell, [{{:atom, 1}, []}, val]}

        {:evaluate, [b, c]} ->
          # Structure: [2 b c] which is really [2 [b c]]
          # This compiles to *[a 2 b c] -> *[*[a b] *[a c]]
          {:cell, [{{:atom, 2}, []}, {:cell, [b, c]}]}

        # For standard Nock constructors, keep them as-is
        {ctor, children} ->
          {ctor, children}
      end
    end

    @doc """
    I convert an extended Nock term to a standard Nock term by first typechecking
    and then compiling any extended constructors down to their raw Nock equivalents.

    I return {:ok, compiled_term} on success or {:error, errors} if typechecking fails.
    """
    @spec compile_to_nock_term(ext_nock_poly_term(v)) ::
            compile_result(v)
          when v: term()
    def compile_to_nock_term(term) do
      # First typecheck the term
      case typecheck(term) do
        :ok ->
          # If valid, apply the algebra to transform the term
          compiled_term =
            Term.eval(compile_algebra(), &Function.identity/1, term)

          {:ok, compiled_term}

        {:error, errors} ->
          {:error, errors}
      end
    end

    @doc """
    I convert an extended Nock term to a standard Nock term, raising an error
    if compilation fails.

    This is a convenience wrapper around compile_to_nock_term that unwraps the
    {:ok, term} result or raises an error with the typecheck failures.
    """
    @spec compile_to_nock_term!(ext_nock_poly_term(v)) ::
            NockTerms.open_nock_poly_term(v)
          when v: term()
    def compile_to_nock_term!(term) do
      case compile_to_nock_term(term) do
        {:ok, compiled} ->
          compiled

        {:error, errors} ->
          raise "Invalid extended Nock term: #{inspect(errors)}"
      end
    end

    @doc """
    I substitute variables in an extended Nock term using the provided substitution
    function, then return the result.

    This is a convenience wrapper around Term.tv_bind/2.
    """
    @spec substitute(ext_nock_poly_term(v), (v -> ext_nock_poly_term(w))) ::
            ext_nock_poly_term(w)
          when v: term(), w: term()
    def substitute(term, f) do
      Term.tv_bind(f, term)
    end

    @type to_noun_result(v) ::
            {:ok, Noun.t()}
            | {:error, ext_nock_compile_error_list(v)}

    @doc """
    I convert an extended Nock term to a standard Nock noun by first compiling
    any extended constructors, then converting to a noun.

    I return {:ok, noun} on success or {:error, errors} if typechecking fails.

    This is the main function for preparing extended terms for execution.
    """
    @spec to_noun(ext_nock_poly_term(v)) :: to_noun_result(v)
          when v: term()
    def to_noun(term) do
      case compile_to_nock_term(term) do
        {:ok, compiled} ->
          {:ok, NockTerms.to_noun(compiled)}

        {:error, errors} ->
          {:error, errors}
      end
    end

    @doc """
    I convert an extended Nock term to a standard Nock noun, raising an error
    if compilation fails.

    This is a convenience wrapper around to_noun that unwraps the
    {:ok, noun} result or raises an error with the typecheck failures.
    """
    @spec to_noun!(ext_nock_poly_term(v)) :: Noun.t()
          when v: term()
    def to_noun!(term) do
      case to_noun(term) do
        {:ok, noun} ->
          noun

        {:error, errors} ->
          raise "Invalid extended Nock term: #{inspect(errors)}"
      end
    end

    @doc """
    I convert an s-expression directly to a Nock noun in one step.

    This is a convenience function that combines from_sexpr and to_noun.
    The s-expression must not contain any variables.
    """
    @spec sexpr_to_noun(nock_poly_sexpr(none())) :: {:ok, Noun.t()} | :error
    def sexpr_to_noun(sexpr) do
      with {:ok, term} <- from_sexpr(sexpr),
           {:ok, noun} <- to_noun(term) do
        {:ok, noun}
      else
        _ -> :error
      end
    end

    @doc """
    I convert an s-expression directly to a Nock noun, raising an error
    if the conversion fails at any step.

    This is a convenience wrapper around sexpr_to_noun that unwraps the
    {:ok, noun} result or raises an error if conversion fails.
    The s-expression must not contain any variables.
    """
    @spec sexpr_to_noun!(nock_poly_sexpr(none())) :: Noun.t()
    def sexpr_to_noun!(sexpr) do
      case sexpr_to_noun(sexpr) do
        {:ok, noun} ->
          noun

        _ ->
          raise "S-expression does not represent a Nock noun: #{inspect(sexpr)}"
      end
    end

    @typedoc """
    I am a convenience S-expression representation for Nock polynomial terms
    with variables of type `v`.

    I can be:
    - A Nock atom (satisfying Noun.is_noun_atom/1)
    - A variable term of the form {:var, v}
    - A list of nock_poly_sexpr(v) (representing a cell node)
    - An extended constructor term of the form {:constructor, [args]}

    The args list must match the arity of the constructor as defined in ext_tspec.
    As extended constructors are added, they will all follow this consistent format.
    """
    @type nock_poly_sexpr(v) ::
            Noun.noun_atom()
            | {:var, v}
            | [nock_poly_sexpr(v)]
            | {ext_term_ctor(), [nock_poly_sexpr(v)]}

    @doc """
    I convert an S-expression representation of a Nock term into an ext_nock_poly_term.

    For a term of the form {:var, v}, I return a variable term.
    For a term which is a Nock atom, I return an atom term.
    For a term which is a list, I recursively convert both the head and tail elements,
    and if both succeed, return a cell term.
    For extended constructors like {:wut, subject}, I handle them appropriately.

    As extended constructors are added, this function will be expanded to handle them.

    I return :error if the term is an empty list or if any recursive conversion fails.
    """
    @spec from_sexpr(nock_poly_sexpr(v)) ::
            {:ok, ext_nock_poly_term(v)} | :error
          when v: term()
    def from_sexpr(sexpr) do
      cond do
        # Variable case
        match?({:var, _}, sexpr) ->
          {:var, v} = sexpr
          {:ok, v}

        # Generic handling for any extended constructor
        is_tuple(sexpr) and tuple_size(sexpr) == 2 and is_atom(elem(sexpr, 0)) and
            is_list(elem(sexpr, 1)) ->
          {ctor, args} = sexpr

          case ext_tspec(ctor) do
            {:ok, expected_arity} ->
              if length(args) != expected_arity do
                :error
              else
                args_result = Enum.map(args, &from_sexpr/1)

                if Enum.any?(args_result, &(&1 == :error)) do
                  :error
                else
                  converted_args =
                    Enum.map(args_result, fn {:ok, term} -> term end)

                  {:ok, {ctor, converted_args}}
                end
              end
          end

        # An empty s-expression is not a valid `ext_nock_poly_term`;
        # a polynomial term always has a constructor
        sexpr == [] ->
          :error

        # Treating a singleton list simply as the element which it contains
        # allows us to avoid special-casing in the multi-element-list case
        # (we treat S-expression lists as improper lists made of nested cells)
        is_list(sexpr) and length(sexpr) == 1 ->
          [head] = sexpr
          from_sexpr(head)

        # As with Nock, a multi-element list is made of nested cells
        is_list(sexpr) ->
          [head | tail] = sexpr

          with {:ok, head_term} <- from_sexpr(head),
               {:ok, tail_term} <- from_sexpr(tail) do
            {:ok, {:cell, [head_term, tail_term]}}
          else
            _ -> :error
          end

        Noun.is_noun_atom(sexpr) ->
          {:ok, {{:atom, sexpr}, []}}

        true ->
          :error
      end
    end

    @doc """
    I convert an S-expression representation of a Nock term into an
    `ext_nock_poly_term`, raising an error if conversion fails.

    I am a convenience wrapper around `from_sexpr/1` that unwraps the
    {:ok, term} result or raises an error if conversion fails.  Because
    the s-expression form is intended for internal convenience, we will
    typically use it in cases where we are explicitly generating expressions
    which are guaranteed to convert successfully to polynomial terms.

    See `from_sexpr/1` for details on the conversion process.
    """
    @spec from_sexpr!(nock_poly_sexpr(v)) :: ext_nock_poly_term(v)
          when v: term()
    def from_sexpr!(sexpr) do
      case from_sexpr(sexpr) do
        {:ok, term} ->
          term

        :error ->
          raise "S-expression does not represent a Nock polynomial term: #{inspect(sexpr)}"
      end
    end
  end
end
