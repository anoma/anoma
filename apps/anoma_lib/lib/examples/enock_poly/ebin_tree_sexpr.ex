defmodule Examples.ENockPoly.EBinTreeSexpr do
  use Memoize

  import ExUnit.Assertions
  import NockPoly.BinTree.MacroDefs
  import NockPoly.Sexpr.MacroDefs

  alias NockPoly.BinTreeSexpr

  def sexpr_to_bintree_nullary() do
    sexpr = sx_atom0(:foo)
    tree = BinTreeSexpr.sexpr_to_bintree(sexpr)
    assert tree == btva(:foo)
    tree
  end

  def sexpr_to_bintree_unary() do
    sexpr = sx_atom(:f, [sx_atom0(:x)])
    tree = BinTreeSexpr.sexpr_to_bintree(sexpr)
    expected = btvp(btva(:f), btva(:x))
    assert tree == expected
    tree
  end

  def sexpr_to_bintree_binary() do
    sexpr = sx_atom(:f, [sx_atom0(:a), sx_atom0(:b)])
    tree = BinTreeSexpr.sexpr_to_bintree(sexpr)
    expected = btvp(btvp(btva(:f), btva(:b)), btva(:a))
    assert tree == expected
    tree
  end

  def sexpr_to_bintree_ternary() do
    sexpr = sx_atom(:f, [sx_atom0(:x), sx_atom0(:y), sx_atom0(:z)])
    tree = BinTreeSexpr.sexpr_to_bintree(sexpr)

    expected =
      btvp(btvp(btvp(btva(:f), btva(:z)), btva(:y)), btva(:x))

    assert tree == expected
    tree
  end

  def bintree_to_sexpr_single_atom() do
    tree = btva(:foo)
    sexpr = BinTreeSexpr.bintree_to_sexpr(tree)
    assert sexpr == sx_atom0(:foo)
    sexpr
  end

  def bintree_to_sexpr_unary() do
    tree = btvp(btva(:f), btva(:x))
    sexpr = BinTreeSexpr.bintree_to_sexpr(tree)
    expected = sx_atom(:f, [sx_atom0(:x)])
    assert sexpr == expected
    sexpr
  end

  def bintree_to_sexpr_binary() do
    tree = btvp(btvp(btva(:f), btva(:b)), btva(:a))
    sexpr = BinTreeSexpr.bintree_to_sexpr(tree)
    expected = sx_atom(:f, [sx_atom0(:a), sx_atom0(:b)])
    assert sexpr == expected
    sexpr
  end

  def bintree_to_sexpr_ternary() do
    tree = btvp(btvp(btvp(btva(:f), btva(:z)), btva(:y)), btva(:x))
    sexpr = BinTreeSexpr.bintree_to_sexpr(tree)
    expected = sx_atom(:f, [sx_atom0(:x), sx_atom0(:y), sx_atom0(:z)])
    assert sexpr == expected
    sexpr
  end

  def sexpr_to_bintree_nested() do
    sexpr =
      sx_atom(:outer, [
        sx_atom(:inner, [sx_atom0(:a), sx_atom0(:b)]),
        sx_atom0(:c)
      ])

    tree = BinTreeSexpr.sexpr_to_bintree(sexpr)

    inner_tree = btvp(btvp(btva(:inner), btva(:b)), btva(:a))

    expected =
      btvp(btvp(btva(:outer), btva(:c)), inner_tree)

    assert tree == expected
    tree
  end

  def bintree_to_sexpr_nested() do
    inner_tree = btvp(btvp(btva(:inner), btva(:b)), btva(:a))
    tree = btvp(btvp(btva(:outer), btva(:c)), inner_tree)

    sexpr = BinTreeSexpr.bintree_to_sexpr(tree)

    expected =
      sx_atom(:outer, [
        sx_atom(:inner, [sx_atom0(:a), sx_atom0(:b)]),
        sx_atom0(:c)
      ])

    assert sexpr == expected
    sexpr
  end

  def roundtrip_sexpr_nullary() do
    sexpr = sx_atom0(:atom)
    result = BinTreeSexpr.roundtrip_sexpr(sexpr)
    assert result == sexpr
    result
  end

  def roundtrip_sexpr_unary() do
    sexpr = sx_atom(:f, [sx_atom0(:x)])
    result = BinTreeSexpr.roundtrip_sexpr(sexpr)
    assert result == sexpr
    result
  end

  def roundtrip_sexpr_binary() do
    sexpr = sx_atom(:f, [sx_atom0(:x), sx_atom0(:y)])
    result = BinTreeSexpr.roundtrip_sexpr(sexpr)
    assert result == sexpr
    result
  end

  def roundtrip_sexpr_nested() do
    sexpr =
      sx_atom(:root, [
        sx_atom(:left, [sx_atom0(:a), sx_atom0(:b)]),
        sx_atom(:right, [sx_atom0(:c)])
      ])

    result = BinTreeSexpr.roundtrip_sexpr(sexpr)
    assert result == sexpr
    result
  end

  def roundtrip_bintree_single_atom() do
    tree = btva(:x)
    result = BinTreeSexpr.roundtrip_bintree(tree)
    assert result == tree
    result
  end

  def roundtrip_bintree_unary() do
    tree = btvp(btva(:f), btva(:x))
    result = BinTreeSexpr.roundtrip_bintree(tree)
    assert result == tree
    result
  end

  def roundtrip_bintree_binary() do
    tree = btvp(btvp(btva(:f), btva(:b)), btva(:a))
    result = BinTreeSexpr.roundtrip_bintree(tree)
    assert result == tree
    result
  end

  def roundtrip_bintree_nested() do
    tree =
      btvp(
        btvp(
          btva(:root),
          btvp(btva(:right), btva(:c))
        ),
        btvp(btvp(btva(:left), btva(:b)), btva(:a))
      )

    result = BinTreeSexpr.roundtrip_bintree(tree)
    assert result == tree
    result
  end

  def closed_sexpr_to_bintree_nullary() do
    closed = sx_closed0(:foo)
    tree = BinTreeSexpr.closed_sexpr_to_bintree(closed)
    assert tree == btva(:foo)
    tree
  end

  def closed_sexpr_to_bintree_unary() do
    closed = sx_closed(:f, [sx_closed0(:x)])
    tree = BinTreeSexpr.closed_sexpr_to_bintree(closed)
    expected = btvp(btva(:f), btva(:x))
    assert tree == expected
    tree
  end

  def closed_sexpr_to_bintree_binary() do
    closed = sx_closed(:f, [:a, :b])
    tree = BinTreeSexpr.closed_sexpr_to_bintree(closed)
    expected = btvp(btvp(btva(:f), btva(:b)), btva(:a))
    assert tree == expected
    tree
  end

  def bintree_to_closed_sexpr_single_atom() do
    tree = btva(:foo)
    closed = BinTreeSexpr.bintree_to_closed_sexpr(tree)
    assert closed == :foo
    closed
  end

  def bintree_to_closed_sexpr_unary() do
    tree = btvp(btva(:f), btva(:x))
    closed = BinTreeSexpr.bintree_to_closed_sexpr(tree)
    expected = {:f, [:x]}
    assert closed == expected
    closed
  end

  def bintree_to_closed_sexpr_binary() do
    tree = btvp(btvp(btva(:f), btva(:b)), btva(:a))
    closed = BinTreeSexpr.bintree_to_closed_sexpr(tree)
    expected = {:f, [:a, :b]}
    assert closed == expected
    closed
  end

  def roundtrip_closed_sexpr_nullary() do
    closed = :atom
    result = BinTreeSexpr.roundtrip_closed_sexpr(closed)
    assert result == closed
    result
  end

  def roundtrip_closed_sexpr_unary() do
    closed = {:f, [:x]}
    result = BinTreeSexpr.roundtrip_closed_sexpr(closed)
    assert result == closed
    result
  end

  def roundtrip_closed_sexpr_binary() do
    closed = {:f, [:a, :b]}
    result = BinTreeSexpr.roundtrip_closed_sexpr(closed)
    assert result == closed
    result
  end

  def roundtrip_closed_sexpr_nested() do
    closed = {:root, [{:left, [:a, :b]}, {:right, [:c]}]}
    result = BinTreeSexpr.roundtrip_closed_sexpr(closed)
    assert result == closed
    result
  end

  def roundtrip_bintree_closed_single_atom() do
    tree = btva(:x)
    result = BinTreeSexpr.roundtrip_bintree_closed(tree)
    assert result == tree
    result
  end

  def roundtrip_bintree_closed_unary() do
    tree = btvp(btva(:f), btva(:x))
    result = BinTreeSexpr.roundtrip_bintree_closed(tree)
    assert result == tree
    result
  end

  def roundtrip_bintree_closed_binary() do
    tree = btvp(btvp(btva(:f), btva(:b)), btva(:a))
    result = BinTreeSexpr.roundtrip_bintree_closed(tree)
    assert result == tree
    result
  end

  def with_variables_sexpr_to_bintree() do
    sexpr = sx_atom(:f, [sx_var(:x), sx_atom0(:y)])
    tree = BinTreeSexpr.sexpr_to_bintree(sexpr)
    expected = btvp(btvp(btva(:f), btva(:y)), btvv(:x))
    assert tree == expected
    tree
  end

  def with_variables_bintree_to_sexpr() do
    tree = btvp(btvp(btva(:f), btva(:y)), btvv(:x))
    sexpr = BinTreeSexpr.bintree_to_sexpr(tree)
    expected = sx_atom(:f, [sx_var(:x), sx_atom0(:y)])
    assert sexpr == expected
    sexpr
  end

  def with_variables_roundtrip_sexpr() do
    sexpr =
      sx_atom(:outer, [
        sx_var(:x),
        sx_atom(:inner, [sx_var(:y), sx_atom0(:z)])
      ])

    result = BinTreeSexpr.roundtrip_sexpr(sexpr)
    assert result == sexpr
    result
  end

  def with_variables_roundtrip_bintree() do
    tree =
      btvp(
        btvp(
          btva(:outer),
          btvp(btvp(btva(:inner), btva(:z)), btvv(:y))
        ),
        btvv(:x)
      )

    result = BinTreeSexpr.roundtrip_bintree(tree)
    assert result == tree
    result
  end
end
