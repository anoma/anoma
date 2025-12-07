defmodule NockPoly.Sexpr do
  @moduledoc """
  I provide a native Elixir S-expression representation for polynomial terms.

  An S-expression is either a variable or a pair of an atom (constructor)
  and a list of S-expressions (children). This is a more natural and readable
  representation than the explicitly tagged `Term.tv` type, allowing for
  compact syntax using native Elixir lists.

  I am parameterized on two types:
  - `atom` - the type of atoms (constructors)
  - `v` - the type of variables

  This type is isomorphic to `Term.tv(atom, v)` for all atom and variable
  types, and the isomorphism is implemented via the `to_term` and `from_term`
  functions.

  ## Examples

      # A variable
      {:var, :x}

      # A nullary constructor (atom with no children)
      {:atom, :foo, []}

      # A unary constructor
      {:atom, :succ, [{:var, :n}]}

      # A binary constructor
      {:atom, :plus, [{:var, :x}, {:var, :y}]}

      # Nested structure
      {:atom, :app, [
        {:atom, :lambda, [{:var, :x}, {:var, :body}]},
        {:atom, :const, [{:var, :value}]}
      ]}
  """

  alias NockPoly.Term

  @typedoc """
  I am an S-expression parameterized on atom and variable types.

  I am either:
  - `{:var, v}` - a variable of type `v`
  - `{:atom, atom, [sexpr(atom, v)]}` - an atom (constructor) with a list of children
  """
  @type sexpr(atom, v) :: {:var, v} | {:atom, atom, [sexpr(atom, v)]}

  @typedoc """
  I am a closed S-expression (with no variables).

  Since there are no variables, I don't need the `:var` tag, and I don't need
  the `:atom` tag to distinguish atoms from variables. I am either:
  - A bare atom (for nullary constructors with no children)
  - A pair of an atom and a non-empty list of children

  The distinction between atoms and tuples makes tags unnecessary, and the
  empty-list case is represented by a bare atom, making the representation
  more concise.

  This is a more convenient representation than `sexpr(atom, none())` because
  it eliminates redundant tagging and empty lists.
  """
  @type closed_sexpr(atom) :: atom | {atom, nonempty_list(closed_sexpr(atom))}

  @doc """
  I create a variable S-expression.

  This is a convenience function for constructing variable S-expressions.
  """
  @spec var(v) :: sexpr(atom, v) when atom: term, v: term
  def var(v) do
    {:var, v}
  end

  @doc """
  I create an atom S-expression with children.

  This is a convenience function for constructing atom S-expressions.
  """
  @spec atom(atom, [sexpr(atom, v)]) :: sexpr(atom, v)
        when atom: term, v: term
  def atom(constructor, children) do
    {:atom, constructor, children}
  end

  @doc """
  I create an atom S-expression with no children.

  This is a convenience function for constructing nullary atom S-expressions.
  """
  @spec atom0(atom) :: sexpr(atom, v) when atom: term, v: term
  def atom0(constructor) do
    {:atom, constructor, []}
  end

  @doc """
  I create a closed S-expression with children.

  This is a convenience function for constructing closed S-expressions
  using the simpler untagged representation. Returns a pair when the
  children list is non-empty.
  """
  @spec closed(atom, nonempty_list(closed_sexpr(atom))) :: closed_sexpr(atom)
        when atom: term
  def closed(constructor, children) do
    {constructor, children}
  end

  @doc """
  I create a closed S-expression with no children.

  This is a convenience function for constructing nullary closed S-expressions.
  Returns a bare atom since there are no children.
  """
  @spec closed0(atom) :: closed_sexpr(atom) when atom: term
  def closed0(constructor) do
    constructor
  end

  defmodule Unreachable do
    @moduledoc """
    I provide unreachable functions for closed S-expressions.

    These functions are used as substitution functions when working with closed
    S-expressions (where the variable type is `none()`). Since `none()` is
    uninhabited, these functions can never actually be called.
    """

    @dialyzer {:nowarn_function, unreachable_sexpr: 1}
    @spec unreachable_sexpr(none()) :: no_return()
    def unreachable_sexpr(var) do
      raise "unreachable: attempted to substitute variable #{inspect(var)} in closed sexpr"
    end
  end

  @typedoc """
  I am a slice algebra for closed S-expressions.

  I provide separate result types for each component of the closed S-expression
  structure:
  - `r_sexpr` - the overall result type for a closed S-expression
  - `r_atom` - the result type for an atom (constructor)
  - `r_list` - the result type for a list of closed S-expressions
  - `r_nelist` - the result type for a non-empty list of closed S-expressions

  The algebra has the following components:

  Atom constructor:
  - `atom` - processes an atom/constructor to produce `r_atom`

  List constructors:
  - `empty` - result for an empty list (constant of type `r_list`)
  - `cons` - builds a cons cell from a sexpr result and a list result
  - `nonempty` - wraps a non-empty list to produce the general list type

  S-expression constructors:
  - `nullary` - processes a nullary constructor (bare atom) to produce `r_sexpr`
  - `nary` - combines an atom result and a non-empty list result to produce `r_sexpr`
  """
  @type closed_sexpr_slice_alg(atom, r_sexpr, r_atom, r_list, r_nelist) :: %{
          atom: (atom -> r_atom),
          empty: r_list,
          cons: (r_sexpr, r_list -> r_nelist),
          nonempty: (r_nelist -> r_list),
          nullary: (r_atom -> r_sexpr),
          nary: (r_atom, r_list -> r_sexpr)
        }

  @doc """
  I am the slice catamorphism for a list of closed S-expressions.

  I recursively fold a list of closed S-expressions by:
  - For the empty list: returning `empty`
  - For a non-empty list: recursively folding the head with `closed_slice_cata`,
    recursively folding the tail with `closed_slice_cata_list`, combining them
    with `cons` to get `r_nelist`, then applying `nonempty` to get `r_list`

  I am mutually recursive with `closed_slice_cata`.
  """
  @spec closed_slice_cata_list(
          [closed_sexpr(atom)],
          closed_sexpr_slice_alg(atom, r_sexpr, r_atom, r_list, r_nelist)
        ) :: r_list
        when atom: term,
             r_sexpr: term,
             r_atom: term,
             r_list: term,
             r_nelist: term
  def closed_slice_cata_list(closed_sexprs, slice_alg) do
    case closed_sexprs do
      [] ->
        slice_alg.empty

      [head | tail] ->
        r_head = closed_slice_cata(head, slice_alg)
        r_tail = closed_slice_cata_list(tail, slice_alg)
        r_nelist = slice_alg.cons.(r_head, r_tail)
        slice_alg.nonempty.(r_nelist)
    end
  end

  @doc """
  I am the slice catamorphism for closed S-expressions.

  I recursively fold a closed S-expression by:
  - For a bare atom (nullary constructor): applying `atom` to get `r_atom`,
    then applying `nullary` to get `r_sexpr`
  - For a pair (n-ary constructor): applying `atom` to get `r_atom`,
    delegating to `closed_slice_cata_list` to fold the children,
    then applying `nary` to get `r_sexpr`

  I am mutually recursive with `closed_slice_cata_list`.
  """
  @spec closed_slice_cata(
          closed_sexpr(atom),
          closed_sexpr_slice_alg(atom, r_sexpr, r_atom, r_list, r_nelist)
        ) :: r_sexpr
        when atom: term,
             r_sexpr: term,
             r_atom: term,
             r_list: term,
             r_nelist: term
  def closed_slice_cata(closed_sexpr, slice_alg) do
    case closed_sexpr do
      constructor when is_atom(constructor) ->
        r_atom = slice_alg.atom.(constructor)
        slice_alg.nullary.(r_atom)

      {constructor, children} ->
        r_atom = slice_alg.atom.(constructor)
        r_list = closed_slice_cata_list(children, slice_alg)
        slice_alg.nary.(r_atom, r_list)
    end
  end

  @spec closed_to_open_slice_alg() ::
          closed_sexpr_slice_alg(
            atom,
            sexpr(atom, none()),
            atom,
            [sexpr(atom, none())],
            [sexpr(atom, none())]
          )
        when atom: term
  defp closed_to_open_slice_alg do
    %{
      atom: fn a -> a end,
      empty: [],
      cons: fn r_sexpr, r_list -> [r_sexpr | r_list] end,
      nonempty: fn nelist -> nelist end,
      nullary: fn a -> {:atom, a, []} end,
      nary: fn a, children -> {:atom, a, children} end
    }
  end

  @doc """
  I convert a list of closed S-expressions to a list of open S-expressions.

  I am implemented using `closed_slice_cata_list`.
  """
  @spec closed_to_open_list([closed_sexpr(atom)]) :: [sexpr(atom, none())]
        when atom: term
  def closed_to_open_list(closed_sexprs) do
    closed_slice_cata_list(closed_sexprs, closed_to_open_slice_alg())
  end

  @doc """
  I convert a closed S-expression to an open S-expression.

  This direction of the isomorphism adds the `:atom` tags to a `closed_sexpr`
  to produce a `sexpr(atom, none())`. A bare atom becomes a nullary constructor,
  and a pair becomes a constructor with children.

  I am implemented using `closed_slice_cata`.
  """
  @spec closed_to_open(closed_sexpr(atom)) :: sexpr(atom, none())
        when atom: term
  def closed_to_open(closed_sexpr) do
    closed_slice_cata(closed_sexpr, closed_to_open_slice_alg())
  end

  @spec from_term_alg({atom, [sexpr(atom, v)]}) :: sexpr(atom, v)
        when atom: term, v: term
  defp from_term_alg({constructor, children}) do
    {:atom, constructor, children}
  end

  @spec from_term_subst(v) :: sexpr(atom, v) when atom: term, v: term
  defp from_term_subst(v) do
    {:var, v}
  end

  @doc """
  I convert a polynomial term to an S-expression.

  This is the inverse direction of the isomorphism between `sexpr(atom, v)`
  and `Term.tv(atom, v)`.
  """
  @spec from_term(Term.tv(atom, v)) :: sexpr(atom, v)
        when atom: term, v: term
  def from_term(term) do
    Term.eval(
      &from_term_alg/1,
      &from_term_subst/1,
      term
    )
  end

  @typedoc """
  I am a slice algebra for S-expressions.

  I provide separate result types for each component of the S-expression structure:
  - `r_sexpr` - the overall result type for an S-expression
  - `r_atom` - the result type for an atom (constructor)
  - `r_list` - the result type for a list of S-expressions
  - `r_nelist` - the result type for a non-empty list of S-expressions

  The algebra has the following components:

  Atom constructor:
  - `atom` - processes an atom/constructor to produce `r_atom`

  List constructors:
  - `empty` - result for an empty list (constant of type `r_list`)
  - `cons` - builds a cons cell from a sexpr result and a list result
  - `nonempty` - wraps a non-empty list to produce the general list type

  S-expression constructor:
  - `sexpr` - combines an atom result and a list result to produce the final sexpr result
  """
  @type sexpr_slice_alg(atom, r_sexpr, r_atom, r_list, r_nelist) :: %{
          atom: (atom -> r_atom),
          empty: r_list,
          cons: (r_sexpr, r_list -> r_nelist),
          nonempty: (r_nelist -> r_list),
          sexpr: (r_atom, r_list -> r_sexpr)
        }

  @doc """
  I am the slice eval morphism for S-expressions with variables.

  I evaluate an S-expression by:
  - For variables: applying the substitution function to get `r_sexpr`
  - For atoms: applying `atom` to get `r_atom`, delegating to `slice_eval_list`
    to build the children list, then applying `sexpr` to get `r_sexpr`

  I am mutually recursive with `slice_eval_list`.
  """
  @spec slice_eval(
          sexpr_slice_alg(atom, r_sexpr, r_atom, r_list, r_nelist),
          (v -> r_sexpr),
          sexpr(atom, v)
        ) :: r_sexpr
        when atom: term,
             v: term,
             r_sexpr: term,
             r_atom: term,
             r_list: term,
             r_nelist: term
  def slice_eval(slice_alg, subst_fn, sexpr) do
    case sexpr do
      {:var, v} ->
        subst_fn.(v)

      {:atom, constructor, children} ->
        r_atom = slice_alg.atom.(constructor)
        r_list = slice_eval_list(slice_alg, subst_fn, children)
        slice_alg.sexpr.(r_atom, r_list)
    end
  end

  @doc """
  I am the slice eval morphism for a list of S-expressions with variables.

  I recursively evaluate a list of S-expressions by:
  - For the empty list: returning `empty`
  - For a non-empty list: recursively evaluating the head with `slice_eval`,
    recursively evaluating the tail with `slice_eval_list`, combining them
    with `cons` to get `r_nelist`, then applying `nonempty` to get `r_list`

  I am mutually recursive with `slice_eval`.
  """
  @spec slice_eval_list(
          sexpr_slice_alg(atom, r_sexpr, r_atom, r_list, r_nelist),
          (v -> r_sexpr),
          [sexpr(atom, v)]
        ) :: r_list
        when atom: term,
             v: term,
             r_sexpr: term,
             r_atom: term,
             r_list: term,
             r_nelist: term
  def slice_eval_list(slice_alg, subst_fn, sexprs) do
    case sexprs do
      [] ->
        slice_alg.empty

      [head | tail] ->
        r_head = slice_eval(slice_alg, subst_fn, head)
        r_tail = slice_eval_list(slice_alg, subst_fn, tail)
        r_nelist = slice_alg.cons.(r_head, r_tail)
        slice_alg.nonempty.(r_nelist)
    end
  end

  @doc """
  I am the slice catamorphism for closed S-expressions.

  I am `slice_eval` specialized to closed S-expressions where the variable
  type is `none()`. Since there are no variables, the variable substitution
  function is never called and can be an unreachable function.
  """
  @spec slice_cata(
          sexpr(atom, none()),
          sexpr_slice_alg(atom, r_sexpr, r_atom, r_list, r_nelist)
        ) :: r_sexpr
        when atom: term,
             r_sexpr: term,
             r_atom: term,
             r_list: term,
             r_nelist: term
  def slice_cata(sexpr, slice_alg) do
    slice_eval(slice_alg, &Unreachable.unreachable_sexpr/1, sexpr)
  end

  @doc """
  I am the slice catamorphism for a list of closed S-expressions.

  I am `slice_eval_list` specialized to closed S-expressions where the variable
  type is `none()`.
  """
  @spec slice_cata_list(
          [sexpr(atom, none())],
          sexpr_slice_alg(atom, r_sexpr, r_atom, r_list, r_nelist)
        ) :: r_list
        when atom: term,
             r_sexpr: term,
             r_atom: term,
             r_list: term,
             r_nelist: term
  def slice_cata_list(sexprs, slice_alg) do
    slice_eval_list(slice_alg, &Unreachable.unreachable_sexpr/1, sexprs)
  end

  @doc """
  I provide the slice algebra for converting S-expressions to terms.

  This algebra converts each component of an S-expression to the corresponding
  term structure, using the same types throughout since terms and S-expressions
  have isomorphic structure.
  """
  @spec to_term_slice_alg() ::
          sexpr_slice_alg(
            atom,
            Term.tv(atom, v),
            atom,
            [Term.tv(atom, v)],
            [Term.tv(atom, v)]
          )
        when atom: term, v: term
  def to_term_slice_alg() do
    %{
      atom: fn constructor -> constructor end,
      empty: [],
      cons: fn term, list -> [term | list] end,
      nonempty: fn nelist -> nelist end,
      sexpr: fn constructor, children ->
        Term.com_tv(constructor, children)
      end
    }
  end

  @doc """
  I convert an S-expression to a polynomial term.

  This is one direction of the isomorphism between `sexpr(atom, v)` and
  `Term.tv(atom, v)`.
  """
  @spec to_term(sexpr(atom, v)) :: Term.tv(atom, v)
        when atom: term, v: term
  def to_term(sexpr) do
    slice_eval(to_term_slice_alg(), &Term.var_tv/1, sexpr)
  end

  @doc """
  I verify that `from_term(to_term(s)) == s` for any S-expression.

  This property demonstrates that the translation is an isomorphism.
  """
  @spec roundtrip_sexpr(sexpr(atom, v)) :: sexpr(atom, v)
        when atom: term, v: term
  def roundtrip_sexpr(sexpr) do
    sexpr |> to_term() |> from_term()
  end

  @doc """
  I verify that `to_term(from_term(t)) == t` for any term.

  This property demonstrates that the translation is an isomorphism.
  """
  @spec roundtrip_term(Term.tv(atom, v)) :: Term.tv(atom, v)
        when atom: term, v: term
  def roundtrip_term(term) do
    term |> from_term() |> to_term()
  end

  @typedoc """
  I am an algebra for S-expressions.

  An algebra maps a constructor and its evaluated children to a result.
  """
  @type sexpr_alg(atom, r) :: ({atom, [r]} -> r)

  @doc """
  I am the `eval` universal morphism for S-expressions with variables.

  I recursively evaluate an S-expression by applying the algebra to each
  constructor along with the results from evaluating its children, and
  applying the substitution function to variables.

  I am implemented by translating the algebra to a slice algebra and
  delegating to `slice_eval`.
  """
  @spec eval(sexpr_alg(atom, r), (v -> r), sexpr(atom, v)) :: r
        when atom: term, v: term, r: term
  def eval(algebra, subst, sexpr) do
    slice_alg = %{
      atom: fn a -> a end,
      empty: [],
      cons: fn r_sexpr, r_list -> [r_sexpr | r_list] end,
      nonempty: fn nelist -> nelist end,
      sexpr: fn atom, children -> algebra.({atom, children}) end
    }

    slice_eval(slice_alg, subst, sexpr)
  end

  @doc """
  I am the catamorphism for closed S-expressions.

  I recursively fold a closed S-expression by applying the algebra to each
  constructor along with the results from folding its children.
  """
  @spec cata(sexpr(atom, none()), sexpr_alg(atom, r)) :: r
        when atom: term, r: term
  def cata(sexpr, algebra) do
    eval(algebra, &Unreachable.unreachable_sexpr/1, sexpr)
  end

  @doc """
  I am the eval morphism for a list of S-expressions with variables.

  I recursively evaluate each S-expression in the list and return the list
  of results.

  I am implemented by translating the algebra to a slice algebra and
  delegating to `slice_eval_list`.
  """
  @spec eval_list(sexpr_alg(atom, r), (v -> r), [sexpr(atom, v)]) :: [r]
        when atom: term, v: term, r: term
  def eval_list(algebra, subst, sexprs) do
    slice_alg = %{
      atom: fn a -> a end,
      empty: [],
      cons: fn r_sexpr, r_list -> [r_sexpr | r_list] end,
      nonempty: fn nelist -> nelist end,
      sexpr: fn atom, children -> algebra.({atom, children}) end
    }

    slice_eval_list(slice_alg, subst, sexprs)
  end

  @doc """
  I am the catamorphism for a list of closed S-expressions.

  I recursively fold each S-expression in the list and return the list
  of results.
  """
  @spec cata_list([sexpr(atom, none())], sexpr_alg(atom, r)) :: [r]
        when atom: term, r: term
  def cata_list(sexprs, algebra) do
    eval_list(algebra, &Unreachable.unreachable_sexpr/1, sexprs)
  end

  @doc """
  I convert an open S-expression with no variables to a closed S-expression.

  This direction of the isomorphism removes the `:atom` tags from a
  `sexpr(atom, none())` to produce a `closed_sexpr(atom)`. A nullary
  constructor (empty children list) becomes a bare atom, and a constructor
  with children becomes a pair.

  Since the type is `sexpr(atom, none())` and `none()` is the empty type,
  there can never be a `:var` case by construction.
  """
  @spec open_to_closed(sexpr(atom, none())) :: closed_sexpr(atom)
        when atom: term
  def open_to_closed(sexpr) do
    cata(
      sexpr,
      fn {constructor, children} ->
        case children do
          [] -> constructor
          _ -> {constructor, children}
        end
      end
    )
  end

  @doc """
  I verify that `open_to_closed(closed_to_open(s)) == s` for any closed S-expression.

  This property demonstrates that the translation is an isomorphism.
  """
  @spec roundtrip_closed(closed_sexpr(atom)) :: closed_sexpr(atom)
        when atom: term
  def roundtrip_closed(closed_sexpr) do
    closed_sexpr |> closed_to_open() |> open_to_closed()
  end

  @doc """
  I verify that `closed_to_open(open_to_closed(s)) == s` for any open S-expression
  with no variables.

  This property demonstrates that the translation is an isomorphism.
  """
  @spec roundtrip_open_closed(sexpr(atom, none())) :: sexpr(atom, none())
        when atom: term
  def roundtrip_open_closed(open_sexpr) do
    open_sexpr |> open_to_closed() |> closed_to_open()
  end

  @doc """
  I compute the depth of an S-expression.

  The depth is 0 for a variable, and 1 + maximum child depth for an atom.
  """
  @spec depth(sexpr(atom, v)) :: non_neg_integer()
        when atom: term, v: term
  def depth(sexpr) do
    eval(
      fn {_atom, child_depths} -> 1 + Enum.max([0 | child_depths]) end,
      fn _var -> 0 end,
      sexpr
    )
  end

  @doc """
  I compute the size of an S-expression.

  The size is 0 for a variable, and 1 + sum of child sizes for an atom.
  """
  @spec size(sexpr(atom, v)) :: non_neg_integer()
        when atom: term, v: term
  def size(sexpr) do
    eval(
      fn {_atom, child_sizes} -> 1 + Enum.sum(child_sizes) end,
      fn _var -> 0 end,
      sexpr
    )
  end

  @doc """
  I map a function over all atoms in an S-expression.

  This is the functor operation in the atom parameter.
  """
  @spec map_atoms((atom1 -> atom2), sexpr(atom1, v)) :: sexpr(atom2, v)
        when atom1: term, atom2: term, v: term
  def map_atoms(f, sexpr) do
    eval(
      fn {constructor, children} -> {:atom, f.(constructor), children} end,
      fn v -> {:var, v} end,
      sexpr
    )
  end

  @doc """
  I map a function over all variables in an S-expression.

  This is the functor operation in the variable parameter.
  """
  @spec map_vars((v1 -> v2), sexpr(atom, v1)) :: sexpr(atom, v2)
        when atom: term, v1: term, v2: term
  def map_vars(f, sexpr) do
    eval(
      fn {constructor, children} -> {:atom, constructor, children} end,
      fn v -> {:var, f.(v)} end,
      sexpr
    )
  end

  @doc """
  I am the bimap for S-expressions, mapping both atom and variable parameters.

  This composes `map_atoms` and `map_vars` into a single operation.
  """
  @spec bimap((atom1 -> atom2), (v1 -> v2), sexpr(atom1, v1)) ::
          sexpr(atom2, v2)
        when atom1: term, atom2: term, v1: term, v2: term
  def bimap(f_atom, f_var, sexpr) do
    map_vars(f_var, map_atoms(f_atom, sexpr))
  end

  @doc """
  I substitute variables in an S-expression.

  Given a substitution function `f : v1 -> sexpr(atom, v2)`, I replace
  all variables in the S-expression according to the function.
  """
  @spec subst((v1 -> sexpr(atom, v2)), sexpr(atom, v1)) :: sexpr(atom, v2)
        when atom: term, v1: term, v2: term
  def subst(f, sexpr) do
    eval(
      fn {constructor, children} -> {:atom, constructor, children} end,
      f,
      sexpr
    )
  end

  @doc """
  I eliminate all variables in an S-expression by substitution.

  This produces an S-expression with no variables from an open one, given a
  substitution function for all variables.

  Note: Despite the name, this returns `sexpr(atom, none())` rather than
  `closed_sexpr(atom)` because it uses the tagged representation. Use
  `open_to_closed` to convert to the untagged representation if needed.
  """
  @spec close((v -> sexpr(atom, none())), sexpr(atom, v)) ::
          sexpr(atom, none())
        when atom: term, v: term
  def close(f, sexpr) do
    subst(f, sexpr)
  end

  defmodule MacroDefs do
    @moduledoc """
    Macro definitions for convenient S-expression construction.
    These are separated into their own module to distinguish them for
    code coverage purposes.
    """

    alias NockPoly.Sexpr

    @doc """
    I create a variable S-expression.

    ## Examples

        iex> import NockPoly.Sexpr.MacroDefs
        iex> sx_var(:x)
        {:var, :x}
    """
    defmacro sx_var(v) do
      quote do
        Sexpr.var(unquote(v))
      end
    end

    @doc """
    I create an atom S-expression with children.

    ## Examples

        iex> import NockPoly.Sexpr.MacroDefs
        iex> sx_atom(:foo, [sx_var(:x)])
        {:atom, :foo, [{:var, :x}]}
    """
    defmacro sx_atom(constructor, children) do
      quote do
        Sexpr.atom(unquote(constructor), unquote(children))
      end
    end

    @doc """
    I create an atom S-expression with no children.

    ## Examples

        iex> import NockPoly.Sexpr.MacroDefs
        iex> sx_atom0(:foo)
        {:atom, :foo, []}
    """
    defmacro sx_atom0(constructor) do
      quote do
        Sexpr.atom0(unquote(constructor))
      end
    end

    @doc """
    I create a closed S-expression with children.

    ## Examples

        iex> import NockPoly.Sexpr.MacroDefs
        iex> sx_closed(:foo, [sx_closed0(:bar)])
        {:foo, [:bar]}
    """
    defmacro sx_closed(constructor, children) do
      quote do
        Sexpr.closed(unquote(constructor), unquote(children))
      end
    end

    @doc """
    I create a closed S-expression with no children.

    ## Examples

        iex> import NockPoly.Sexpr.MacroDefs
        iex> sx_closed0(:foo)
        :foo
    """
    defmacro sx_closed0(constructor) do
      quote do
        Sexpr.closed0(unquote(constructor))
      end
    end
  end
end
