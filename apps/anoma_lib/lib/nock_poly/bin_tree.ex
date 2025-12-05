defmodule NockPoly.BinTree do
  @moduledoc """
  I represent labeled binary trees as the free monad of the product functor.

  A binary tree is either an atom (leaf) or a pair of binary trees (branch).
  This corresponds to the initial algebra of the bifunctor
  `BinTreeF(atom, x) = Either atom (Product x x)`, which is the free monad
  of the product functor `ProductMonad(x) = (x, x)`.

  The free monad of the product functor provides a natural representation
  of binary trees with labels at the leaves. This module provides universal
  morphisms and derived operations for working with such trees.

  I am parameterized on an atom type so that different contexts may choose
  different representations of leaf labels.

  The correspondence to fixed points of `termf` (from `NockPoly.Term`) is
  via snoclist interpretation: a list of binary trees (viewed as a snoclist)
  corresponds to a single binary tree. This correspondence is implemented
  separately.
  """

  @typedoc """
  I represent an either type for atom or variable labels.

  This allows representing `bintreefv(atom, v, x)` as `bintreef(either(atom, v), x)`,
  and `btv(atom, v)` as `bt(either(atom, v))`.
  """
  @type either(atom, v) :: {:btatom, atom} | {:btvar, v}

  @doc "I am the morphism-map component of the bifunctor `either` in the atom parameter."
  @spec either_map_atom((atom1 -> atom2), either(atom1, v)) ::
          either(atom2, v)
        when atom1: term, atom2: term, v: term
  def either_map_atom(fa, either) do
    case either do
      {:btatom, a} -> {:btatom, fa.(a)}
      {:btvar, v} -> {:btvar, v}
    end
  end

  @doc "I am the morphism-map component of the bifunctor `either` in the variable parameter."
  @spec either_map_var((v -> w), either(atom, v)) :: either(atom, w)
        when atom: term, v: term, w: term
  def either_map_var(fv, either) do
    case either do
      {:btatom, a} -> {:btatom, a}
      {:btvar, v} -> {:btvar, fv.(v)}
    end
  end

  @doc "I am the bimap for the bifunctor `either`, mapping both atom and variable parameters."
  @spec either_bimap((atom1 -> atom2), (v -> w), either(atom1, v)) ::
          either(atom2, w)
        when atom1: term, atom2: term, v: term, w: term
  def either_bimap(fa, fv, either) do
    either_map_var(fv, either_map_atom(fa, either))
  end

  @typedoc """
  I generate a binary tree structure parameterized on atom and recursive types.

  I am the bifunctor `BinTreeF(atom, x) = Either atom (Product x x)`.
  """
  @type bintreef(atom, x) :: {:atom, atom} | {:pair, x, x}

  @doc "I am the morphism-map component of the bifunctor `bintreef` in the atom parameter."
  @spec bintreef_map_atom((atom1 -> atom2), bintreef(atom1, x)) ::
          bintreef(atom2, x)
        when atom1: term, atom2: term, x: term
  def bintreef_map_atom(fa, tree) do
    case tree do
      {:atom, ea} ->
        {:atom, fa.(ea)}

      {:pair, left, right} ->
        {:pair, left, right}
    end
  end

  @doc "I am the morphism-map component of the bifunctor `bintreef` in the recursive parameter."
  @spec bintreef_map((a -> b), bintreef(atom, a)) :: bintreef(atom, b)
        when atom: term, a: term, b: term
  def bintreef_map(f, tree) do
    case tree do
      {:atom, ea} ->
        {:atom, ea}

      {:pair, left, right} ->
        {:pair, f.(left), f.(right)}
    end
  end

  @doc "I am the bimap for the bifunctor `bintreef`, mapping both atom and recursive parameters."
  @spec bintreef_bimap((atom1 -> atom2), (a -> b), bintreef(atom1, a)) ::
          bintreef(atom2, b)
        when atom1: term, atom2: term, a: term, b: term
  def bintreef_bimap(fa, fx, tree) do
    bintreef_map(fx, bintreef_map_atom(fa, tree))
  end

  @typedoc "Type of algebras of `bintreef`."
  @type bintree_alg(atom, x) :: (bintreef(atom, x) -> x)

  @typedoc """
  I represent the product functor (product monad) on a type.

  The product monad takes `x` to `(x, x)`.
  """
  @type product(x) :: {x, x}

  @typedoc "Type of algebras of the product functor."
  @type product_alg(x) :: (product(x) -> x)

  @typedoc """
  I generate open binary trees: trees potentially containing variables.

  I am the translate functor for the free monad of the product functor.
  For a binary tree, this is `BinTreeTrF(atom, v, x) = v + atom + (x, x)`,
  which can be written as `Either v (BinTreeF(atom, x))`.

  I am implemented as `bintreef(either(atom, v), x)`, where the `{:btvar, v}`
  and `{:btatom, atom}` cases are combined into `{:atom, either(atom, v)}`.
  """
  @type bintreefv(atom, v, x) :: bintreef(either(atom, v), x)

  @doc "I am the morphism-map component of the trifunctor `bintreefv` in the atom parameter."
  @spec bintreefv_map_atom((atom1 -> atom2), bintreefv(atom1, v, x)) ::
          bintreefv(atom2, v, x)
        when atom1: term, atom2: term, v: term, x: term
  def bintreefv_map_atom(fa, tree) do
    case tree do
      {:atom, {:btvar, v}} ->
        {:atom, {:btvar, v}}

      {:atom, {:btatom, ea}} ->
        {:atom, {:btatom, fa.(ea)}}

      {:pair, left, right} ->
        {:pair, left, right}
    end
  end

  @doc "I am the morphism-map component of the trifunctor `bintreefv` in the variable parameter."
  @spec bintreefv_map_var((v -> w), bintreefv(atom, v, x)) ::
          bintreefv(atom, w, x)
        when atom: term, v: term, w: term, x: term
  def bintreefv_map_var(fv, tree) do
    case tree do
      {:atom, {:btvar, v}} ->
        {:atom, {:btvar, fv.(v)}}

      {:atom, {:btatom, ea}} ->
        {:atom, {:btatom, ea}}

      {:pair, left, right} ->
        {:pair, left, right}
    end
  end

  @doc "I am the morphism-map component of the trifunctor `bintreefv` in the recursive parameter."
  @spec bintreefv_map((x -> y), bintreefv(atom, v, x)) ::
          bintreefv(atom, v, y)
        when atom: term, v: term, x: term, y: term
  def bintreefv_map(fx, tree) do
    case tree do
      {:atom, {:btvar, v}} ->
        {:atom, {:btvar, v}}

      {:atom, {:btatom, ea}} ->
        {:atom, {:btatom, ea}}

      {:pair, left, right} ->
        {:pair, fx.(left), fx.(right)}
    end
  end

  @doc "I am the bimap for the trifunctor `bintreefv`, mapping variable and recursive parameters."
  @spec bintreefv_bimap((v -> w), (x -> y), bintreefv(atom, v, x)) ::
          bintreefv(atom, w, y)
        when atom: term, v: term, w: term, x: term, y: term
  def bintreefv_bimap(fv, fx, tree) do
    bintreefv_map(fx, bintreefv_map_var(fv, tree))
  end

  @doc "I am the trimap for the trifunctor `bintreefv`, mapping atom, variable, and recursive parameters."
  @spec bintreefv_trimap(
          (atom1 -> atom2),
          (v -> w),
          (x -> y),
          bintreefv(atom1, v, x)
        ) :: bintreefv(atom2, w, y)
        when atom1: term, atom2: term, v: term, w: term, x: term, y: term
  def bintreefv_trimap(fa, fv, fx, tree) do
    bintreefv_map(fx, bintreefv_map_var(fv, bintreefv_map_atom(fa, tree)))
  end

  @typedoc """
  I am a closed labeled binary tree (with no variables).

  I am the carrier of the initial algebra of `bintreef(atom)`, which
  is the free monad of the product functor with atom labels.
  """
  @type bt(atom) :: {:in_bt, bintreef(atom, bt(atom))}

  @doc """
  I am the action of the initial algebra of `bintreef(atom)`.

  My implementation is trivial; I exist to make explicit that `bt`
  is an algebra.
  """
  @spec in_bt(bintreef(atom, bt(atom))) :: bt(atom) when atom: term
  def in_bt(x) do
    {:in_bt, x}
  end

  @doc """
  I am the inverse of the action of the initial algebra of `bintreef(atom)`.

  I correspond to pattern-matching. Being the inverse of an algebra, I am
  a coalgebra.
  """
  @spec out_bt(bt(atom)) :: bintreef(atom, bt(atom)) when atom: term
  def out_bt({:in_bt, x}) do
    x
  end

  @typedoc """
  I am a labeled binary tree with variables.

  I am the carrier of the initial algebra of `bintreefv(atom, v)`, which
  is the free monad of the product functor with atom labels. As a type
  constructor in the variable parameter, I am the free monad of `bintreef`.

  I am implemented as `bt(either(atom, v))`, the fixed point of
  `bintreef(either(atom, v), x)`.
  """
  @type btv(atom, v) :: bt(either(atom, v))

  @doc """
  I am the action of the initial algebra of `bintreefv(atom, v)`.

  I am implemented as `in_bt`.
  """
  @spec in_btv(bintreefv(atom, v, btv(atom, v))) :: btv(atom, v)
        when atom: term, v: term
  def in_btv(x) do
    in_bt(x)
  end

  @doc """
  I am the inverse of the action of the initial algebra of `bintreefv(atom, v)`.

  I correspond to pattern-matching. I am implemented as `out_bt`.
  """
  @spec out_btv(btv(atom, v)) :: bintreefv(atom, v, btv(atom, v))
        when atom: term, v: term
  def out_btv(tree) do
    out_bt(tree)
  end

  @doc "I create a variable term of type `bintreefv` by wrapping a variable."
  @spec var_bintreefv(v) :: bintreefv(atom, v, x)
        when atom: term, v: term, x: term
  def var_bintreefv(v) do
    {:atom, {:btvar, v}}
  end

  @doc "I create an atom term of type `bintreefv` by wrapping an atom."
  @spec atom_bintreefv(atom) :: bintreefv(atom, v, x)
        when atom: term, v: term, x: term
  def atom_bintreefv(ea) do
    {:atom, {:btatom, ea}}
  end

  @doc "I create a pair term of type `bintreefv` by wrapping two children."
  @spec pair_bintreefv(x, x) :: bintreefv(atom, v, x)
        when atom: term, v: term, x: term
  def pair_bintreefv(left, right) do
    {:pair, left, right}
  end

  @doc "I create a variable term of type `btv` by composing `in_btv` with `var_bintreefv`."
  @spec var_btv(v) :: btv(atom, v) when atom: term, v: term
  def var_btv(v) do
    in_btv(var_bintreefv(v))
  end

  @doc "I create an atom term of type `btv` by composing `in_btv` with `atom_bintreefv`."
  @spec atom_btv(atom) :: btv(atom, v) when atom: term, v: term
  def atom_btv(ea) do
    in_btv(atom_bintreefv(ea))
  end

  @doc "I create a pair term of type `btv` by composing `in_btv` with `pair_bintreefv`."
  @spec pair_btv(btv(atom, v), btv(atom, v)) :: btv(atom, v)
        when atom: term, v: term
  def pair_btv(left, right) do
    in_btv(pair_bintreefv(left, right))
  end

  @typedoc "An open binary tree with Nock atom labels and variables of type `v`."
  @type nock_atom_btv(v) :: btv(Noun.noun_atom(), v)

  @typedoc "An open binary tree with Nock noun labels and variables of type `v`."
  @type nock_noun_btv(v) :: btv(Noun.t(), v)

  @typedoc "A closed binary tree with Nock atom labels."
  @type nock_atom_bt :: bt(Noun.noun_atom())

  @typedoc "A closed binary tree with Nock noun labels."
  @type nock_noun_bt :: bt(Noun.t())

  @doc """
  I am the `eval` universal morphism for the free monad of binary trees.

  I am the right adjunct of the free/forgetful adjunction between the
  category of algebras of `bintreef` and the base category.

  Because the free monad of a binary tree is isomorphic to a binary tree
  with an `Either v atom` atom type, this is a binary tree catamorphism
  specialized to handle variables.

  I am implemented by translating the algebra to a slice algebra and
  delegating to `slice_eval`.
  """
  @spec eval(bintree_alg(atom, r), (v -> r), btv(atom, v)) :: r
        when atom: term, v: term, r: term
  def eval(algebra, subst, tree) do
    slice_alg = %{
      atom: fn ea -> ea end,
      pair: fn left_r, right_r -> {left_r, right_r} end,
      from_atom: fn ea -> algebra.({:atom, ea}) end,
      from_pair: fn {left_r, right_r} ->
        algebra.({:pair, left_r, right_r})
      end
    }

    slice_eval(slice_alg, subst, tree)
  end

  defmodule Unreachable do
    @moduledoc """
    I contain a function expected to be unreachable, factored out as a module
    so that test coverage can ignore it.
    """

    @dialyzer {:nowarn_function, unreachable_var: 1}
    @spec unreachable_var(none()) :: no_return()
    def unreachable_var(var) do
      raise "Variable in closed binary tree encountered: #{inspect(var)}"
    end
  end

  @doc """
  I am the catamorphism for closed binary trees.

  I recursively fold the tree by applying the given algebra function to
  each constructor along with the results from folding its children.
  """
  @spec cata(bt(atom), bintree_alg(atom, r)) :: r
        when atom: term, r: term
  def cata(tree, algebra) do
    eval(algebra, &Unreachable.unreachable_var/1, tree)
  end

  @typedoc """
  I am a slice algebra for binary trees.

  I provide separate result types for:
  - `r_bt`: the overall binary tree result
  - `r_atom`: the result for atom leaves
  - `r_pair`: the result for pair nodes

  The `from_atom` and `from_pair` functions are the slice morphisms that
  project from each constructor type to the overall result type.
  """
  @type bintree_slice_alg(atom, r_bt, r_atom, r_pair) :: %{
          atom: (atom -> r_atom),
          pair: (r_bt, r_bt -> r_pair),
          from_atom: (r_atom -> r_bt),
          from_pair: (r_pair -> r_bt)
        }

  @doc """
  I am the slice eval morphism for binary trees with variables.

  I recursively evaluate an open binary tree by:
  - For variables: applying the substitution function to get `r_bt`
  - For atoms: applying `atom` to get `r_atom`, then `from_atom` to get `r_bt`
  - For pairs: delegating to `slice_eval_pair` to get `r_pair`, then
    `from_pair` to get `r_bt`

  I am mutually recursive with `slice_eval_pair`.
  """
  @spec slice_eval(
          bintree_slice_alg(atom, r_bt, r_atom, r_pair),
          (v -> r_bt),
          btv(atom, v)
        ) :: r_bt
        when atom: term, v: term, r_bt: term, r_atom: term, r_pair: term
  def slice_eval(slice_alg, subst, tree) do
    case out_btv(tree) do
      {:atom, {:btvar, v}} ->
        subst.(v)

      {:atom, {:btatom, ea}} ->
        slice_alg.from_atom.(slice_alg.atom.(ea))

      {:pair, left, right} ->
        pair_r = slice_eval_pair(slice_alg, subst, left, right)
        slice_alg.from_pair.(pair_r)
    end
  end

  @doc """
  I am the slice catamorphism for closed binary trees.

  I am `slice_eval` specialized to closed trees where the variable type is `none()`.
  """
  @spec slice_cata(
          bt(atom),
          bintree_slice_alg(atom, r_bt, r_atom, r_pair)
        ) :: r_bt
        when atom: term, r_bt: term, r_atom: term, r_pair: term
  def slice_cata(tree, slice_alg) do
    slice_eval(slice_alg, &Unreachable.unreachable_var/1, tree)
  end

  @doc """
  I am the slice eval morphism for a pair of binary trees with variables.

  I am a convenience wrapper around `slice_eval` that:
  1. Recursively evaluates both trees to get `r_bt` results
  2. Applies the `pair` component to get `r_pair`

  This is useful when the caller is primarily interested in the pair structure
  rather than the overall tree structure.
  """
  @spec slice_eval_pair(
          bintree_slice_alg(atom, r_bt, r_atom, r_pair),
          (v -> r_bt),
          btv(atom, v),
          btv(atom, v)
        ) :: r_pair
        when atom: term, v: term, r_bt: term, r_atom: term, r_pair: term
  def slice_eval_pair(slice_alg, subst, left_tree, right_tree) do
    left_r = slice_eval(slice_alg, subst, left_tree)
    right_r = slice_eval(slice_alg, subst, right_tree)
    slice_alg.pair.(left_r, right_r)
  end

  @doc """
  I am the slice catamorphism for a pair of closed binary trees.

  I am a convenience wrapper around `slice_cata` that:
  1. Recursively evaluates both trees to get `r_bt` results
  2. Applies the `pair` component to get `r_pair`

  This is useful when performing simultaneous induction on pairs of closed binary trees.
  """
  @spec slice_cata_pair(
          bt(atom),
          bt(atom),
          bintree_slice_alg(atom, r_bt, r_atom, r_pair)
        ) :: r_pair
        when atom: term, r_bt: term, r_atom: term, r_pair: term
  def slice_cata_pair(left_tree, right_tree, slice_alg) do
    slice_eval_pair(
      slice_alg,
      &Unreachable.unreachable_var/1,
      left_tree,
      right_tree
    )
  end

  @doc """
  I am the catamorphism specialized for product algebras on closed binary trees.

  This evaluates a binary tree (free monad of the product functor) to a
  type using a product algebra (monoid structure on pairs).
  """
  @spec prod_eval_mon(product_alg(v), btv(v, none())) :: v when v: term
  def prod_eval_mon(prod_alg, tree) do
    algebra = fn
      {:atom, a} -> a
      {:pair, l, r} -> prod_alg.({l, r})
    end

    cata(tree, algebra)
  end

  @spec btamap_alg((atom1 -> atom2), bintreef(atom1, btv(atom2, v))) ::
          btv(atom2, v)
        when atom1: term, atom2: term, v: term
  def btamap_alg(f, tree) do
    case tree do
      {:atom, ea} ->
        atom_btv(f.(ea))

      {:pair, left, right} ->
        pair_btv(left, right)
    end
  end

  @doc """
  I map a function over the atoms in a binary tree.

  I am the morphism-map component of the functor `btv` in its atom parameter.
  """
  @spec btamap((atom1 -> atom2), btv(atom1, v)) :: btv(atom2, v)
        when atom1: term, atom2: term, v: term
  def btamap(f, tree) do
    eval(&btamap_alg(f, &1), &Function.identity/1, tree)
  end

  @spec btvmap_alg(bintreef(atom, btv(atom, b))) :: btv(atom, b)
        when atom: term, b: term
  def btvmap_alg(tree) do
    case tree do
      {:atom, ea} ->
        atom_btv(ea)

      {:pair, left, right} ->
        pair_btv(left, right)
    end
  end

  @spec btvmap_subst((a -> b), a) :: btv(atom, b)
        when atom: term, a: term, b: term
  def btvmap_subst(f, x) do
    var_btv(f.(x))
  end

  @doc """
  I map a function over the variables in a binary tree.

  I am the morphism-map component of the functor `btv` in its variable parameter.
  """
  @spec btvmap((a -> b), btv(atom, a)) :: btv(atom, b)
        when atom: term, a: term, b: term
  def btvmap(f, tree) do
    eval(&btvmap_alg/1, &btvmap_subst(f, &1), tree)
  end

  @doc """
  I am the comultiplication (duplicate) for the free monad `btv`.

  Given a tree of type `btv(atom, v)`, I return a tree of type
  `btv(atom, btv(atom, v))`.
  """
  @spec btv_comult(btv(atom, v)) :: btv(atom, btv(atom, v))
        when atom: term, v: term
  def btv_comult(tree) do
    btvmap(&var_btv/1, tree)
  end

  @doc """
  I am the join (multiplication) for the free monad `btv`.

  Given a tree of type `btv(atom, btv(atom, v))`, I return a tree of type
  `btv(atom, v)` by flattening the structure.
  """
  @spec btv_mult(btv(atom, btv(atom, v))) :: btv(atom, v)
        when atom: term, v: term
  def btv_mult(tree) do
    eval(&btvmap_alg/1, &Function.identity/1, tree)
  end

  @doc """
  I am the monadic bind for the free monad `btv`.

  The bind of a free monad implements substitution of variables with
  arbitrary trees.
  """
  @spec btv_bind((v -> btv(atom, w)), btv(atom, v)) :: btv(atom, w)
        when atom: term, v: term, w: term
  def btv_bind(f, tree) do
    btv_mult(btvmap(f, tree))
  end

  @doc """
  I substitute all variables in an open binary tree with closed binary trees.

  This produces a closed binary tree by eliminating all variables.
  """
  @spec full_subst((v -> bt(atom)), btv(atom, v)) :: bt(atom)
        when atom: term, v: term
  def full_subst(subst, tree) do
    btv_bind(subst, tree)
  end

  @doc """
  I return the maximum depth of the binary tree as a natural number.

  Depth is 1 for an atom, 0 for a variable, and 1 + max of children depths
  for a pair.
  """
  @spec depth(btv(atom, v)) :: non_neg_integer() when atom: term, v: term
  def depth(tree) do
    eval(
      fn
        {:atom, _ea} -> 1
        {:pair, left_depth, right_depth} -> 1 + max(left_depth, right_depth)
      end,
      fn _var -> 0 end,
      tree
    )
  end

  @doc """
  I return the total number of atoms in the binary tree.

  Variables contribute 0 to the count.
  """
  @spec size(btv(atom, v)) :: non_neg_integer() when atom: term, v: term
  def size(tree) do
    eval(
      fn
        {:atom, _ea} -> 1
        {:pair, left_size, right_size} -> left_size + right_size
      end,
      fn _var -> 0 end,
      tree
    )
  end

  defmodule MacroDefs do
    @moduledoc """
    I provide macro definitions for binary tree construction, separated into
    a module so that test coverage can ignore them.
    """

    alias NockPoly.BinTree

    @doc """
    I create a variable term of type `bintreefv`.

    ## Examples

        iex> import NockPoly.BinTree.MacroDefs
        iex> btfv(42)
        {:atom, {:btvar, 42}}
    """
    defmacro btfv(var) do
      quote do
        BinTree.var_bintreefv(unquote(var))
      end
    end

    @doc """
    I create an atom term of type `bintreefv`.

    ## Examples

        iex> import NockPoly.BinTree.MacroDefs
        iex> btfa(:foo)
        {:atom, {:btatom, :foo}}
    """
    defmacro btfa(atom) do
      quote do
        BinTree.atom_bintreefv(unquote(atom))
      end
    end

    @doc """
    I create a pair term of type `bintreefv`.

    ## Examples

        iex> import NockPoly.BinTree.MacroDefs
        iex> btfp(btfa(:a), btfa(:b))
        {:pair, {:atom, {:btatom, :a}}, {:atom, {:btatom, :b}}}
    """
    defmacro btfp(left, right) do
      quote do
        BinTree.pair_bintreefv(unquote(left), unquote(right))
      end
    end

    @doc """
    I create a variable term of type `btv`.

    ## Examples

        iex> import NockPoly.BinTree.MacroDefs
        iex> btvv(42)
        {:in_bt, {:atom, {:btvar, 42}}}
    """
    defmacro btvv(var) do
      quote do
        BinTree.var_btv(unquote(var))
      end
    end

    @doc """
    I create an atom term of type `btv`.

    ## Examples

        iex> import NockPoly.BinTree.MacroDefs
        iex> btva(:foo)
        {:in_bt, {:atom, {:btatom, :foo}}}
    """
    defmacro btva(atom) do
      quote do
        BinTree.atom_btv(unquote(atom))
      end
    end

    @doc """
    I create a pair term of type `btv`.

    ## Examples

        iex> import NockPoly.BinTree.MacroDefs
        iex> btvp(btva(:a), btva(:b))
        {:in_bt,
         {:pair, {:in_bt, {:atom, {:btatom, :a}}},
          {:in_bt, {:atom, {:btatom, :b}}}}}
    """
    defmacro btvp(left, right) do
      quote do
        BinTree.pair_btv(unquote(left), unquote(right))
      end
    end
  end
end
