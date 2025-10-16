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
  I convert a term with list of children to a binary tree via snoclist interpretation.

  The list of children is interpreted as a snoclist: the rightmost element
  becomes the rightmost leaf in the tree, and we build the tree right-associatively.

  An empty children list creates just an atom node.
  A pair `(atom, [x, y, z])` builds the tree structure
  `(((atom . z) . y) . x)` where each `.` is a pair.
  """
  @spec term_to_bintree(Term.t(ctor)) :: BinTree.bt(ctor) when ctor: term
  def term_to_bintree(term) do
    Term.eval(
      &term_to_bintree_alg/1,
      &Term.Unreachable.unreachable_term/1,
      term
    )
  end

  @doc """
  I convert a term with variables to a binary tree with variables.

  This is the generalization of `term_to_bintree` that preserves variables.
  """
  @spec termv_to_bintreev(Term.tv(ctor, v)) :: BinTree.btv(ctor, v)
        when ctor: term, v: term
  def termv_to_bintreev(term) do
    Term.eval(&term_to_bintree_alg/1, &BinTree.var_btv/1, term)
  end

  @spec term_to_bintree_alg({ctor, [BinTree.btv(ctor, v)]}) ::
          BinTree.btv(ctor, v)
        when ctor: term, v: term
  defp term_to_bintree_alg({ctor, children}) do
    list_to_bintree_snoclist(ctor, children)
  end

  @spec list_to_bintree_snoclist(ctor, [BinTree.btv(ctor, v)]) ::
          BinTree.btv(ctor, v)
        when ctor: term, v: term
  defp list_to_bintree_snoclist(ctor, children) do
    reversed = Enum.reverse(children)
    build_tree_from_snoclist(BinTree.atom_btv(ctor), reversed)
  end

  @spec build_tree_from_snoclist(BinTree.btv(ctor, v), [BinTree.btv(ctor, v)]) ::
          BinTree.btv(ctor, v)
        when ctor: term, v: term
  defp build_tree_from_snoclist(acc, []) do
    acc
  end

  defp build_tree_from_snoclist(acc, [head | tail]) do
    new_acc = BinTree.pair_btv(acc, head)
    build_tree_from_snoclist(new_acc, tail)
  end

  @doc """
  I convert a binary tree to a term extracting the snoclist structure.

  This is the inverse of `term_to_bintree`. The binary tree is deconstructed
  into an atom and a list of children, where the list represents the snoclist
  structure embedded in the tree.
  """
  @spec bintree_to_term(BinTree.bt(ctor)) :: Term.t(ctor) when ctor: term
  def bintree_to_term(tree) do
    BinTree.cata(tree, &bintree_to_term_alg/1)
  end

  @doc """
  I convert a binary tree with variables to a term with variables.

  This is the generalization of `bintree_to_term` that preserves variables.
  """
  @spec bintreev_to_termv(BinTree.btv(ctor, v)) :: Term.tv(ctor, v)
        when ctor: term, v: term
  def bintreev_to_termv(tree) do
    BinTree.eval(&bintree_to_term_alg/1, &Term.var_tv/1, tree)
  end

  @spec bintree_to_term_alg(BinTree.bintreef(ctor, Term.tv(ctor, v))) ::
          Term.tv(ctor, v)
        when ctor: term, v: term
  defp bintree_to_term_alg(tree_f) do
    case tree_f do
      {:atom, ctor} ->
        Term.com_tv(ctor, [])

      {:pair, left, right} ->
        bintree_pair_to_term(left, right)
    end
  end

  @spec bintree_pair_to_term(Term.tv(ctor, v), Term.tv(ctor, v)) ::
          Term.tv(ctor, v)
        when ctor: term, v: term
  defp bintree_pair_to_term(left_term, right_child) do
    case Term.out_tv(left_term) do
      {:tcom, {ctor, children}} ->
        Term.com_tv(ctor, [right_child | children])

      {:tvar, _v} ->
        left_term
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
