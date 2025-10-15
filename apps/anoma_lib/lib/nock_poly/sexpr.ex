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
  the `:atom` tag to distinguish atoms from variables. I am simply an atom
  together with a list of children.

  This is a more convenient representation than `sexpr(atom, none())` because
  it eliminates redundant tagging.
  """
  @type closed_sexpr(atom) :: {atom, [closed_sexpr(atom)]}

  @typedoc """
  I am an S-expression with Nock atom labels.
  """
  @type nock_atom_sexpr(v) :: sexpr(Noun.noun_atom(), v)

  @typedoc """
  I am an S-expression with Nock noun labels.
  """
  @type nock_noun_sexpr(v) :: sexpr(Noun.t(), v)

  @typedoc """
  I am a closed S-expression with Nock atom labels.
  """
  @type closed_nock_atom_sexpr :: closed_sexpr(Noun.noun_atom())

  @typedoc """
  I am a closed S-expression with Nock noun labels.
  """
  @type closed_nock_noun_sexpr :: closed_sexpr(Noun.t())

  @doc """
  I convert a closed S-expression to an open S-expression.

  This direction of the isomorphism adds the `:atom` tags to a `closed_sexpr`
  to produce a `sexpr(atom, none())`.
  """
  @spec closed_to_open(closed_sexpr(atom)) :: sexpr(atom, none())
        when atom: term
  def closed_to_open({constructor, children}) do
    open_children = Enum.map(children, &closed_to_open/1)
    {:atom, constructor, open_children}
  end

  @doc """
  I convert an open S-expression with no variables to a closed S-expression.

  This direction of the isomorphism removes the `:atom` tags from a
  `sexpr(atom, none())` to produce a `closed_sexpr(atom)`.

  This function assumes the input has no `:var` nodes (since the type is
  `sexpr(atom, none())`). If a `:var` node is encountered, it will raise
  an error.
  """
  @spec open_to_closed(sexpr(atom, none())) :: closed_sexpr(atom)
        when atom: term
  def open_to_closed(sexpr) do
    case sexpr do
      {:atom, constructor, children} ->
        closed_children = Enum.map(children, &open_to_closed/1)
        {constructor, closed_children}

      {:var, _v} ->
        raise "Unexpected variable in closed S-expression"
    end
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
  I convert an S-expression to a polynomial term.

  This is one direction of the isomorphism between `sexpr(atom, v)` and
  `Term.tv(atom, v)`.
  """
  @spec to_term(sexpr(atom, v)) :: Term.tv(atom, v)
        when atom: term, v: term
  def to_term(sexpr) do
    case sexpr do
      {:var, v} ->
        Term.var_tv(v)

      {:atom, constructor, children} ->
        term_children = Enum.map(children, &to_term/1)
        Term.com_tv(constructor, term_children)
    end
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
  using the simpler untagged representation.
  """
  @spec closed(atom, [closed_sexpr(atom)]) :: closed_sexpr(atom)
        when atom: term
  def closed(constructor, children) do
    {constructor, children}
  end

  @doc """
  I create a closed S-expression with no children.

  This is a convenience function for constructing nullary closed S-expressions.
  """
  @spec closed0(atom) :: closed_sexpr(atom) when atom: term
  def closed0(constructor) do
    {constructor, []}
  end

  @doc """
  I compute the depth of an S-expression.

  The depth is 0 for a variable, and 1 + maximum child depth for an atom.
  """
  @spec depth(sexpr(atom, v)) :: non_neg_integer()
        when atom: term, v: term
  def depth(sexpr) do
    case sexpr do
      {:var, _v} ->
        0

      {:atom, _constructor, children} ->
        case children do
          [] -> 1
          _ -> 1 + Enum.max(Enum.map(children, &depth/1))
        end
    end
  end

  @doc """
  I compute the size of an S-expression.

  The size is 0 for a variable, and 1 + sum of child sizes for an atom.
  """
  @spec size(sexpr(atom, v)) :: non_neg_integer()
        when atom: term, v: term
  def size(sexpr) do
    case sexpr do
      {:var, _v} ->
        0

      {:atom, _constructor, children} ->
        1 + Enum.sum(Enum.map(children, &size/1))
    end
  end

  @doc """
  I map a function over all atoms in an S-expression.

  This is the functor operation in the atom parameter.
  """
  @spec map_atoms((atom1 -> atom2), sexpr(atom1, v)) :: sexpr(atom2, v)
        when atom1: term, atom2: term, v: term
  def map_atoms(f, sexpr) do
    case sexpr do
      {:var, v} ->
        {:var, v}

      {:atom, constructor, children} ->
        {:atom, f.(constructor), Enum.map(children, &map_atoms(f, &1))}
    end
  end

  @doc """
  I map a function over all variables in an S-expression.

  This is the functor operation in the variable parameter.
  """
  @spec map_vars((v1 -> v2), sexpr(atom, v1)) :: sexpr(atom, v2)
        when atom: term, v1: term, v2: term
  def map_vars(f, sexpr) do
    case sexpr do
      {:var, v} ->
        {:var, f.(v)}

      {:atom, constructor, children} ->
        {:atom, constructor, Enum.map(children, &map_vars(f, &1))}
    end
  end

  @doc """
  I substitute variables in an S-expression.

  Given a substitution function `f : v1 -> sexpr(atom, v2)`, I replace
  all variables in the S-expression according to the function.
  """
  @spec subst((v1 -> sexpr(atom, v2)), sexpr(atom, v1)) :: sexpr(atom, v2)
        when atom: term, v1: term, v2: term
  def subst(f, sexpr) do
    case sexpr do
      {:var, v} ->
        f.(v)

      {:atom, constructor, children} ->
        {:atom, constructor, Enum.map(children, &subst(f, &1))}
    end
  end

  @doc """
  I eliminate all variables in an S-expression by substitution.

  This produces a closed S-expression from an open one, given a substitution
  function for all variables.
  """
  @spec close((v -> closed_sexpr(atom)), sexpr(atom, v)) ::
          closed_sexpr(atom)
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
        {:foo, [{:bar, []}]}
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
        {:foo, []}
    """
    defmacro sx_closed0(constructor) do
      quote do
        Sexpr.closed0(unquote(constructor))
      end
    end
  end
end
