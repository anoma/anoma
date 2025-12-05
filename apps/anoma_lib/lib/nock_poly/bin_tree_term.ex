defmodule NockPoly.BinTreeTerm do
  @moduledoc """
  I provide the correspondence between labeled binary trees and terms.

  A binary tree can be viewed as an S-expression consisting of an atom
  together with a list of child S-expressions. Treating this list as a
  snoclist (reversed list) establishes a correspondence with the fixed
  point of the `termf` functor from `NockPoly.Term`.

  The correspondence relies on the fact that `termf(ctor, x) = {ctor, [x]}` represents
  "constructor with list of children", which when viewed as a snoclist
  corresponds exactly to the structure of a binary tree built right-
  associatively via repeated pairing.

  This correspondence is an isomorphism: the translations in both
  directions are mutual inverses.
  """

  alias NockPoly.BinTree
  alias NockPoly.Term

  @doc """
  I provide the slice algebra for converting terms to binary trees with left-associativity.

  This algebra interprets the children list as a snoclist: the rightmost element
  becomes the rightmost leaf in the tree, and we build the tree left-associatively.

  An empty children list creates just an atom node.
  A term `(ctor, [x, y, z])` builds the tree structure
  `(((ctor . z) . y) . x)` where each `.` is a pair.

  The algebra uses functions to build up the tree structure without explicit recursion:
  - Lists of terms return functions `(ctor -> BinTree.btv(ctor, v))`
  - Empty list returns `fn ctor -> atom(ctor)` (base case)
  - Cons combines a tree with a list function to produce a new function
  - The term handler applies the function to the constructor to get the final tree
  """
  def term_to_bintree_slice_alg_left() do
    %{
      ctor: fn ctor -> ctor end,
      empty: fn ctor -> BinTree.atom_btv(ctor) end,
      cons: fn tree, list_fn ->
        fn ctor -> BinTree.pair_btv(list_fn.(ctor), tree) end
      end,
      nonempty: fn nelist_fn -> nelist_fn end,
      term: fn ctor, list_fn ->
        list_fn.(ctor)
      end
    }
  end

  @doc """
  I provide the slice algebra for converting terms to binary trees with right-associativity.

  This algebra interprets the children list as a regular list: the leftmost element
  is processed first, and we build the tree right-associatively.

  An empty children list creates just an atom node.
  A term `(ctor, [x, y, z])` builds the tree structure
  `(ctor . (x . (y . z)))` where each `.` is a pair.

  The algebra uses functions to build up the tree structure without explicit recursion:
  - Lists of terms return functions `(tree -> BinTree.btv(ctor, v))`
  - Empty list returns the identity function (base case)
  - Cons combines a tree with a list function to chain the pairing
  - The term handler applies the function to the constructor atom to get the final tree
  """
  def term_to_bintree_slice_alg_right() do
    %{
      ctor: fn ctor -> ctor end,
      empty: fn tree -> tree end,
      cons: fn tree, list_fn ->
        fn base -> BinTree.pair_btv(base, list_fn.(tree)) end
      end,
      nonempty: fn nelist_fn -> nelist_fn end,
      term: fn ctor, list_fn ->
        list_fn.(BinTree.atom_btv(ctor))
      end
    }
  end

  @doc """
  I convert a term with list of children to a binary tree via snoclist interpretation.

  The list of children is interpreted as a snoclist: the rightmost element
  becomes the rightmost leaf in the tree, and we build the tree left-associatively.

  An empty children list creates just an atom node.
  A pair `(atom, [x, y, z])` builds the tree structure
  `(((atom . z) . y) . x)` where each `.` is a pair.
  """
  @spec term_to_bintree(Term.t(ctor)) :: BinTree.bt(ctor) when ctor: term
  def term_to_bintree(term) do
    Term.slice_cata(term, term_to_bintree_slice_alg_left())
  end

  @doc """
  I convert a term with variables to a binary tree with variables (left-associative).

  This is the generalization of `term_to_bintree` that preserves variables.
  """
  @spec termv_to_bintreev(Term.tv(ctor, v)) :: BinTree.btv(ctor, v)
        when ctor: term, v: term
  def termv_to_bintreev(term) do
    Term.slice_eval(
      term_to_bintree_slice_alg_left(),
      &BinTree.var_btv/1,
      term
    )
  end

  @doc """
  I convert a term to a binary tree with right-associativity.

  A term `(ctor, [x, y, z])` builds the tree structure
  `(ctor . (x . (y . z)))` where each `.` is a pair.
  """
  @spec term_to_bintree_right(Term.t(ctor)) :: BinTree.bt(ctor)
        when ctor: term
  def term_to_bintree_right(term) do
    Term.slice_cata(term, term_to_bintree_slice_alg_right())
  end

  @doc """
  I convert a term with variables to a binary tree with variables (right-associative).

  A term `(ctor, [x, y, z])` builds the tree structure
  `(ctor . (x . (y . z)))` where each `.` is a pair.
  """
  @spec termv_to_bintreev_right(Term.tv(ctor, v)) :: BinTree.btv(ctor, v)
        when ctor: term, v: term
  def termv_to_bintreev_right(term) do
    Term.slice_eval(
      term_to_bintree_slice_alg_right(),
      &BinTree.var_btv/1,
      term
    )
  end

  @spec bintree_pair_to_term_result_left(
          {:ok, Term.tv(ctor, v)} | {:error, nonempty_list(term())},
          {:ok, Term.tv(ctor, v)} | {:error, nonempty_list(term())}
        ) :: {:ok, Term.tv(ctor, v)} | {:error, nonempty_list(term())}
        when ctor: term, v: term
  defp bintree_pair_to_term_result_left(left_result, right_result) do
    with {:ok, left_term} <- left_result,
         {:ok, right_child} <- right_result do
      case Term.out_tv(left_term) do
        {:tcom, {ctor, children}} ->
          {:ok, Term.com_tv(ctor, [right_child | children])}

        {:tvar, v} ->
          {:error, [{:variable_in_application_position, v, right_child}]}
      end
    end
  end

  @doc """
  I provide the slice algebra for converting binary trees to terms (left-associative).

  This algebra extracts the snoclist structure from a binary tree and
  converts it to a term with a constructor and list of children.

  This is the internal version that returns `{:ok, term}` or `{:error, errors}`
  to handle the case where a variable appears in application position, which
  cannot be represented as a term.
  """
  @spec bintree_to_term_slice_alg_result_left() ::
          BinTree.bintree_slice_alg(
            ctor,
            {:ok, Term.tv(ctor, v)} | {:error, nonempty_list(term())},
            ctor,
            {:ok, Term.tv(ctor, v)} | {:error, nonempty_list(term())}
          )
        when ctor: term, v: term
  def bintree_to_term_slice_alg_result_left() do
    %{
      atom: fn ctor -> ctor end,
      pair: &bintree_pair_to_term_result_left/2,
      from_atom: fn ctor -> {:ok, Term.com_tv(ctor, [])} end,
      from_pair: &Function.identity/1
    }
  end

  @type role :: :as_child | :in_spine
  @type role_fn(ctor, v) ::
          (role ->
             {:ok, Term.tv(ctor, v)}
             | {:ok, [Term.tv(ctor, v)]}
             | {:error, nonempty_list(term())})

  defp bintree_pair_to_role_fn_right(left_fn, right_fn) do
    fn role ->
      case role do
        :as_child ->
          with {:ok, left_child} <- left_fn.(:as_child),
               {:ok, right_spine} <- right_fn.(:in_spine) do
            case Term.out_tv(left_child) do
              {:tcom, {ctor, []}} ->
                {:ok, Term.com_tv(ctor, right_spine)}

              {:tcom, {_ctor, _children}} ->
                {:error,
                 [{:non_nullary_term_in_constructor_position, left_child}]}

              {:tvar, v} ->
                {:error, [{:variable_in_constructor_position, v}]}
            end
          end

        :in_spine ->
          with {:ok, left_child} <- left_fn.(:as_child),
               {:ok, right_spine} <- right_fn.(:in_spine) do
            {:ok, [left_child | right_spine]}
          end
      end
    end
  end

  @doc """
  I provide the slice algebra for converting binary trees to terms (right-associative).

  This algebra uses higher-order functions to distinguish between different roles:
  - The return type is a function `(role -> result)` where role is `:as_child` or `:in_spine`
  - An atom returns a function that:
    - In `:as_child` role: returns a nullary term with that constructor
    - In `:in_spine` role: returns a singleton list containing a nullary term
  - A pair returns a function that:
    - In `:as_child` role: builds a term by extracting ctor from left, children from right
    - In `:in_spine` role: prepends the left child to the right spine's list

  By using function-valued returns, we defer the decision about how to interpret
  a tree until we know its role in the parent context.
  """
  @spec bintree_to_term_slice_alg_result_right_fn() ::
          BinTree.bintree_slice_alg(
            ctor,
            role_fn(ctor, none()),
            ctor,
            role_fn(ctor, none())
          )
        when ctor: term
  def bintree_to_term_slice_alg_result_right_fn() do
    %{
      atom: &Function.identity/1,
      pair: &bintree_pair_to_role_fn_right/2,
      from_atom: fn ctor ->
        fn role ->
          case role do
            :as_child -> {:ok, Term.com_tv(ctor, [])}
            :in_spine -> {:ok, [Term.com_tv(ctor, [])]}
          end
        end
      end,
      from_pair: fn role_fn -> role_fn end
    }
  end

  @spec bintreev_to_termv_slice_alg_result_right_fn() ::
          BinTree.bintree_slice_alg(
            ctor,
            role_fn(ctor, v),
            ctor | {:btvar, v},
            role_fn(ctor, v)
          )
        when ctor: term, v: term
  def bintreev_to_termv_slice_alg_result_right_fn() do
    %{
      # atom function receives either a raw constructor or {:btvar, v}
      # but slice_eval unwraps {:btatom, ctor} to just ctor before calling atom
      # So atom function receives: ctor (when from {:btatom, ctor}) or gets variable handling separately
      # Actually, looking at slice_eval more carefully:
      # {:atom, {:btvar, v}} -> subst.(v)  -- variables go to subst
      # {:atom, {:btatom, ea}} -> from_atom.(atom.(ea))  -- atoms get unwrapped
      # So atom function only receives raw constructors!
      atom: &Function.identity/1,
      pair: &bintree_pair_to_role_fn_right/2,
      from_atom: fn ctor ->
        # ctor is a raw constructor value (not wrapped)
        fn role ->
          case role do
            :as_child ->
              {:ok, Term.com_tv(ctor, [])}

            :in_spine ->
              {:ok, [Term.com_tv(ctor, [])]}
          end
        end
      end,
      from_pair: fn role_fn ->
        # Just return the function as-is; it already handles both roles
        role_fn
      end
    }
  end

  @doc """
  I test whether a binary tree with variables can be interpreted as a term.

  A binary tree can be interpreted as a term if and only if no variable
  appears in application position (i.e., as the left child of a pair).
  """
  @spec bintreev_interpretable_as_termv?(BinTree.btv(ctor, v)) :: boolean()
        when ctor: term, v: term
  def bintreev_interpretable_as_termv?(tree) do
    result =
      BinTree.slice_eval(
        bintree_to_term_slice_alg_result_left(),
        fn _v -> {:ok, Term.var_tv(:placeholder)} end,
        tree
      )

    match?({:ok, _}, result)
  end

  @doc """
  I convert a binary tree to a term extracting the snoclist structure (left-associative).

  This is the inverse of `term_to_bintree`. The binary tree is deconstructed
  into an atom and a list of children, where the list represents the snoclist
  structure embedded in the tree.

  Since closed trees cannot contain variables, the conversion always succeeds.
  """
  @spec bintree_to_term(BinTree.bt(ctor)) :: Term.t(ctor) when ctor: term
  def bintree_to_term(tree) do
    {:ok, term} =
      BinTree.slice_cata(tree, bintree_to_term_slice_alg_result_left())

    term
  end

  @doc """
  I convert a binary tree with variables to a term with variables (left-associative).

  This conversion is only defined for binary trees where no variable appears
  in application position (i.e., as the left child of a pair). If such a
  variable is encountered, this function raises an error.

  Use `bintreev_interpretable_as_termv?/1` to test whether a conversion
  will succeed before calling this function.
  """
  @spec bintreev_to_termv(BinTree.btv(ctor, v)) :: Term.tv(ctor, v)
        when ctor: term, v: term
  def bintreev_to_termv(tree) do
    case BinTree.slice_eval(
           bintree_to_term_slice_alg_result_left(),
           fn v -> {:ok, Term.var_tv(v)} end,
           tree
         ) do
      {:ok, term} ->
        term

      {:error, [{:variable_in_application_position, v, right_child}]} ->
        raise ArgumentError,
              "Cannot convert binary tree to term: variable #{inspect(v)} " <>
                "appears in application position with argument #{inspect(right_child)}"
    end
  end

  @doc """
  I convert a binary tree to a term extracting the right-associative list structure.

  This is the inverse of `term_to_bintree_right`. The binary tree is deconstructed
  into an atom and a list of children, where the list is built right-associatively.

  Since closed trees cannot contain variables, the conversion always succeeds.

  This implementation uses a slice algebra with function-valued return types to
  distinguish between constructor position and child position without explicit recursion.
  """
  @spec bintree_to_term_right(BinTree.bt(ctor)) :: Term.t(ctor)
        when ctor: term
  def bintree_to_term_right(tree) do
    role_fn =
      BinTree.slice_cata(tree, bintree_to_term_slice_alg_result_right_fn())

    case role_fn.(:as_child) do
      {:ok, term} ->
        term

      {:error, [{:non_nullary_term_in_constructor_position, term}]} ->
        raise ArgumentError,
              "Cannot convert binary tree to term: non-nullary term #{inspect(term)} " <>
                "appears in constructor position"
    end
  end

  @doc """
  I convert a binary tree with variables to a term with variables (right-associative).

  This conversion is only defined for binary trees where no variable or non-nullary term
  appears in constructor position (i.e., as the left child of a pair). If such a
  term is encountered, this function raises an error.

  Use `bintreev_interpretable_as_termv_right?/1` to test whether a conversion
  will succeed before calling this function.

  This implementation uses a slice algebra with function-valued return types to
  distinguish between constructor position and child position without explicit recursion.
  """
  @spec bintreev_to_termv_right(BinTree.btv(ctor, v)) :: Term.tv(ctor, v)
        when ctor: term, v: term
  def bintreev_to_termv_right(tree) do
    role_fn =
      BinTree.slice_eval(
        bintreev_to_termv_slice_alg_result_right_fn(),
        fn v ->
          fn role ->
            case role do
              :as_child -> {:ok, Term.var_tv(v)}
              :in_spine -> {:ok, [Term.var_tv(v)]}
            end
          end
        end,
        tree
      )

    case role_fn.(:as_child) do
      {:ok, term} ->
        term

      {:error, [{:non_nullary_term_in_constructor_position, term}]} ->
        raise ArgumentError,
              "Cannot convert binary tree to term: non-nullary term #{inspect(term)} " <>
                "appears in constructor position"

      {:error, [{:variable_in_constructor_position, v}]} ->
        raise ArgumentError,
              "Cannot convert binary tree to term: variable #{inspect(v)} " <>
                "appears in constructor position"
    end
  end

  @doc """
  I test whether a binary tree with variables can be interpreted as a term (right-associative).

  A binary tree can be interpreted as a right-associative term if and only if no variable
  or non-nullary term appears in constructor position (i.e., as the left child of a pair).
  """
  @spec bintreev_interpretable_as_termv_right?(BinTree.btv(ctor, v)) ::
          boolean()
        when ctor: term, v: term
  def bintreev_interpretable_as_termv_right?(tree) do
    role_fn =
      BinTree.slice_eval(
        bintreev_to_termv_slice_alg_result_right_fn(),
        fn v ->
          fn role ->
            case role do
              :as_child -> {:ok, Term.var_tv(v)}
              :in_spine -> {:ok, [Term.var_tv(v)]}
            end
          end
        end,
        tree
      )

    case role_fn.(:as_child) do
      {:ok, _term} -> true
      {:error, _} -> false
    end
  end

  @doc """
  I verify that bintree_to_term(term_to_bintree(t)) equals t for a closed term.
  """
  @spec roundtrip_term(Term.t(ctor)) :: Term.t(ctor) when ctor: term
  def roundtrip_term(term) do
    term |> term_to_bintree() |> bintree_to_term()
  end

  @doc """
  I verify that term_to_bintree(bintree_to_term(bt)) equals bt for a closed binary tree.
  """
  @spec roundtrip_bintree(BinTree.bt(ctor)) :: BinTree.bt(ctor)
        when ctor: term
  def roundtrip_bintree(tree) do
    tree |> bintree_to_term() |> term_to_bintree()
  end

  @doc """
  I verify that bintreev_to_termv(termv_to_bintreev(t)) equals t for an open term.
  """
  @spec roundtrip_termv(Term.tv(ctor, v)) :: Term.tv(ctor, v)
        when ctor: term, v: term
  def roundtrip_termv(term) do
    term |> termv_to_bintreev() |> bintreev_to_termv()
  end

  @doc """
  I verify that termv_to_bintreev(bintreev_to_termv(bt)) equals bt for an open binary tree.
  """
  @spec roundtrip_bintreev(BinTree.btv(ctor, v)) :: BinTree.btv(ctor, v)
        when ctor: term, v: term
  def roundtrip_bintreev(tree) do
    tree |> bintreev_to_termv() |> termv_to_bintreev()
  end
end
