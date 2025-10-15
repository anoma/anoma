defmodule NockPoly.BinTreeSexpr do
  @moduledoc """
  I provide translations between binary trees and S-expressions.

  I compose the existing isomorphisms between binary trees and polynomial
  terms (`NockPoly.BinTreeTerm`) with the isomorphism between polynomial
  terms and S-expressions (`NockPoly.Sexpr`). Both use snoclist
  interpretation for lists.

  ## Examples

      # Snoclist interpretation
      sexpr_to_bintree({:atom, :f, [{:atom, :a, []}, {:atom, :b, []}]})
      # => btvp(btvp(btva(:f), btva(:b)), btva(:a))

  ## Closed S-expressions

  For convenience, I also provide functions that work directly with
  `closed_sexpr` by composing with the `closed_to_open`/`open_to_closed`
  conversions from `NockPoly.Sexpr`.
  """

  alias NockPoly.BinTree
  alias NockPoly.BinTreeTerm
  alias NockPoly.Sexpr

  @doc """
  I convert an S-expression to a binary tree.

  I compose `Sexpr.to_term` with `BinTreeTerm.termv_to_bintreev`.
  """
  @spec sexpr_to_bintree(Sexpr.sexpr(ctor, v)) :: BinTree.btv(ctor, v)
        when ctor: term, v: term
  def sexpr_to_bintree(sexpr) do
    sexpr
    |> Sexpr.to_term()
    |> BinTreeTerm.termv_to_bintreev()
  end

  @doc """
  I convert a binary tree to an S-expression.

  I compose `BinTreeTerm.bintreev_to_termv` with `Sexpr.from_term`.
  """
  @spec bintree_to_sexpr(BinTree.btv(ctor, v)) :: Sexpr.sexpr(ctor, v)
        when ctor: term, v: term
  def bintree_to_sexpr(tree) do
    tree
    |> BinTreeTerm.bintreev_to_termv()
    |> Sexpr.from_term()
  end

  @doc """
  I convert a closed S-expression to a binary tree.

  This is a convenience function that composes `closed_to_open` with
  `sexpr_to_bintree`.
  """
  @spec closed_sexpr_to_bintree(Sexpr.closed_sexpr(ctor)) :: BinTree.bt(ctor)
        when ctor: term
  def closed_sexpr_to_bintree(closed_sexpr) do
    closed_sexpr |> Sexpr.closed_to_open() |> sexpr_to_bintree()
  end

  @doc """
  I convert a binary tree to a closed S-expression.

  This is a convenience function that composes `bintree_to_sexpr` with
  `open_to_closed`.
  """
  @spec bintree_to_closed_sexpr(BinTree.bt(ctor)) :: Sexpr.closed_sexpr(ctor)
        when ctor: term
  def bintree_to_closed_sexpr(tree) do
    tree |> bintree_to_sexpr() |> Sexpr.open_to_closed()
  end

  @doc """
  I verify that converting from S-expression to binary tree and back is
  the identity.
  """
  @spec roundtrip_sexpr(Sexpr.sexpr(ctor, v)) :: Sexpr.sexpr(ctor, v)
        when ctor: term, v: term
  def roundtrip_sexpr(sexpr) do
    sexpr |> sexpr_to_bintree() |> bintree_to_sexpr()
  end

  @doc """
  I verify that converting from binary tree to S-expression and back is
  the identity.
  """
  @spec roundtrip_bintree(BinTree.btv(ctor, v)) :: BinTree.btv(ctor, v)
        when ctor: term, v: term
  def roundtrip_bintree(tree) do
    tree |> bintree_to_sexpr() |> sexpr_to_bintree()
  end

  @doc """
  I verify that converting from closed S-expression to binary tree and
  back is the identity.
  """
  @spec roundtrip_closed_sexpr(Sexpr.closed_sexpr(ctor)) ::
          Sexpr.closed_sexpr(ctor)
        when ctor: term
  def roundtrip_closed_sexpr(closed_sexpr) do
    closed_sexpr |> closed_sexpr_to_bintree() |> bintree_to_closed_sexpr()
  end

  @doc """
  I verify that converting from binary tree to closed S-expression and
  back is the identity.
  """
  @spec roundtrip_bintree_closed(BinTree.bt(ctor)) :: BinTree.bt(ctor)
        when ctor: term
  def roundtrip_bintree_closed(tree) do
    tree |> bintree_to_closed_sexpr() |> closed_sexpr_to_bintree()
  end
end
