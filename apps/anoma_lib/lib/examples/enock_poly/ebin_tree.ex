defmodule Examples.ENockPoly.EBinTree do
  use Memoize

  import ExUnit.Assertions
  import NockPoly.BinTree.MacroDefs

  alias NockPoly.BinTree

  def basic_atom_term() do
    tree = btva(:foo)
    assert tree == {:in_btv, {:btatom, :foo}}
    tree
  end

  def basic_variable_term() do
    tree = btvv(42)
    assert tree == {:in_btv, {:btvar, 42}}
    tree
  end

  def basic_pair_term() do
    tree = btvp(btva(:a), btva(:b))

    assert tree ==
             {:in_btv,
              {:btpair, {:in_btv, {:btatom, :a}}, {:in_btv, {:btatom, :b}}}}

    tree
  end

  def basic_nested_pair() do
    tree = btvp(btvp(btva(:a), btva(:b)), btva(:c))

    assert tree ==
             {:in_btv,
              {:btpair,
               {:in_btv,
                {:btpair, {:in_btv, {:btatom, :a}}, {:in_btv, {:btatom, :b}}}},
               {:in_btv, {:btatom, :c}}}}

    tree
  end

  def functor_bintreef_map_over_atom() do
    tree_f = {:atom, :foo}
    result = BinTree.bintreef_map(&String.to_atom("mapped_#{&1}"), tree_f)
    assert result == {:atom, :foo}
    result
  end

  def functor_bintreef_map_over_pair() do
    tree_f = {:pair, 1, 2}
    result = BinTree.bintreef_map(&(&1 * 10), tree_f)
    assert result == {:pair, 10, 20}
    result
  end

  def functor_bintreefv_bimap_over_variable() do
    tree_fv = {:btvar, 42}
    result = BinTree.bintreefv_bimap(&(&1 + 1), &(&1 * 10), tree_fv)
    assert result == {:btvar, 43}
    result
  end

  def functor_bintreefv_bimap_over_atom() do
    tree_fv = {:btatom, :foo}

    result = BinTree.bintreefv_bimap(&(&1 + 1), &(&1 * 10), tree_fv)

    assert result == {:btatom, :foo}
    result
  end

  def functor_bintreefv_bimap_over_pair() do
    tree_fv = {:btpair, 1, 2}
    result = BinTree.bintreefv_bimap(&(&1 + 1), &(&1 * 10), tree_fv)
    assert result == {:btpair, 10, 20}
    result
  end

  def algebra_in_btv_and_out_btv_are_inverses() do
    tree_fv = {:btatom, :test}
    tree = BinTree.in_btv(tree_fv)
    assert BinTree.out_btv(tree) == tree_fv
    tree
  end

  def eval_on_atom_tree() do
    tree = btva(:foo)

    algebra = fn {:atom, a} -> "atom: #{a}" end

    subst = fn v -> "var: #{v}" end

    result = BinTree.eval(algebra, subst, tree)
    assert result == "atom: foo"
    result
  end

  def eval_on_variable_tree() do
    tree = btvv(42)

    algebra = fn _ -> raise "Should not be called on variable tree" end

    subst = fn v -> "var: #{v}" end

    result = BinTree.eval(algebra, subst, tree)
    assert result == "var: 42"
    result
  end

  def eval_on_pair_tree() do
    tree = btvp(btva(:a), btva(:b))

    algebra = fn
      {:atom, a} -> "atom: #{a}"
      {:pair, l, r} -> "pair: #{l}, #{r}"
    end

    subst = fn v -> "var: #{v}" end

    result = BinTree.eval(algebra, subst, tree)
    assert result == "pair: atom: a, atom: b"
    result
  end

  def eval_on_nested_pair_tree() do
    tree = btvp(btvp(btva(:a), btva(:b)), btva(:c))

    algebra = fn
      {:atom, a} -> "atom: #{a}"
      {:pair, l, r} -> "(#{l} . #{r})"
    end

    subst = fn v -> "var: #{v}" end

    result = BinTree.eval(algebra, subst, tree)
    assert result == "((atom: a . atom: b) . atom: c)"
    result
  end

  def eval_with_variable_substitution() do
    tree = btvp(btvv(1), btvp(btva(:x), btvv(2)))

    algebra = fn
      {:atom, a} -> "#{a}"
      {:pair, l, r} -> "[#{l} #{r}]"
    end

    subst = fn v -> "var#{v}" end

    result = BinTree.eval(algebra, subst, tree)
    assert result == "[var1 [x var2]]"
    result
  end

  def cata_on_atom_tree() do
    tree = btva(:test)

    algebra = fn {:atom, a} -> {:leaf, a} end

    result = BinTree.cata(tree, algebra)
    assert result == {:leaf, :test}
    result
  end

  def cata_on_pair_tree() do
    tree = btvp(btva(:a), btva(:b))

    algebra = fn
      {:atom, a} -> {:leaf, a}
      {:pair, l, r} -> {:node, l, r}
    end

    result = BinTree.cata(tree, algebra)
    assert result == {:node, {:leaf, :a}, {:leaf, :b}}
    result
  end

  def cata_sum_of_integer_atoms() do
    tree = btvp(btvp(btva(1), btva(2)), btva(3))

    algebra = fn
      {:atom, a} -> a
      {:pair, l, r} -> l + r
    end

    result = BinTree.cata(tree, algebra)
    assert result == 6
    result
  end

  def prod_eval_mon_simple_tree() do
    tree = btvp(btva(10), btva(20))
    prod_alg = fn {l, r} -> l + r end

    result = BinTree.prod_eval_mon(prod_alg, tree)
    assert result == 30
    result
  end

  def prod_eval_mon_nested_tree() do
    tree = btvp(btvp(btva(1), btva(2)), btva(3))
    prod_alg = fn {l, r} -> l + r end

    result = BinTree.prod_eval_mon(prod_alg, tree)
    assert result == 6
    result
  end

  def prod_eval_mon_concatenation() do
    tree = btvp(btvp(btva("a"), btva("b")), btva("c"))
    prod_alg = fn {l, r} -> l <> r end

    result = BinTree.prod_eval_mon(prod_alg, tree)
    assert result == "abc"
    result
  end

  def btamap_map_over_atoms() do
    tree = btvp(btva(:foo), btva(:bar))
    result = BinTree.btamap(&String.to_atom("x_#{&1}"), tree)

    expected = btvp(btva(:x_foo), btva(:x_bar))
    assert result == expected
    result
  end

  def btamap_preserves_structure() do
    tree = btvp(btvp(btva(1), btva(2)), btva(3))
    result = BinTree.btamap(&(&1 * 10), tree)

    expected = btvp(btvp(btva(10), btva(20)), btva(30))
    assert result == expected
    result
  end

  def btvmap_map_over_variables() do
    tree = btvp(btvv(1), btvv(2))
    result = BinTree.btvmap(&(&1 + 10), tree)

    expected = btvp(btvv(11), btvv(12))
    assert result == expected
    result
  end

  def btvmap_preserves_atoms() do
    tree = btvp(btvv(1), btva(:foo))
    result = BinTree.btvmap(&(&1 * 2), tree)

    expected = btvp(btvv(2), btva(:foo))
    assert result == expected
    result
  end

  def btvmap_nested_tree() do
    tree = btvp(btvp(btvv(1), btvv(2)), btvv(3))
    result = BinTree.btvmap(&(&1 + 100), tree)

    expected = btvp(btvp(btvv(101), btvv(102)), btvv(103))
    assert result == expected
    result
  end

  def btv_comult_creates_nested_structure() do
    tree = btvp(btva(:a), btvv(1))
    result = BinTree.btv_comult(tree)

    expected = btvp(btva(:a), btvv(btvv(1)))
    assert result == expected
    result
  end

  def btv_mult_flattens_nested_structure() do
    tree = btvp(btva(:a), btvv(btvv(1)))
    result = BinTree.btv_mult(tree)

    expected = btvp(btva(:a), btvv(1))
    assert result == expected
    result
  end

  def mult_after_comult_is_identity() do
    tree = btvp(btvp(btva(:a), btva(:b)), btvv(1))
    result = BinTree.btv_mult(BinTree.btv_comult(tree))
    assert result == tree
    result
  end

  def bind_substitutes_variables_with_trees() do
    tree = btvp(btvv(1), btvv(2))

    subst = fn
      1 -> btva(:first)
      2 -> btva(:second)
    end

    result = BinTree.btv_bind(subst, tree)
    expected = btvp(btva(:first), btva(:second))
    assert result == expected
    result
  end

  def bind_with_complex_substitution() do
    tree = btvp(btvv(1), btva(:x))

    subst = fn 1 -> btvp(btva(:a), btva(:b)) end

    result = BinTree.btv_bind(subst, tree)
    expected = btvp(btvp(btva(:a), btva(:b)), btva(:x))
    assert result == expected
    result
  end

  def bind_with_nested_tree() do
    tree = btvp(btvv(1), btvp(btvv(2), btva(:z)))

    subst = fn
      1 -> btva(:one)
      2 -> btva(:two)
    end

    result = BinTree.btv_bind(subst, tree)
    expected = btvp(btva(:one), btvp(btva(:two), btva(:z)))
    assert result == expected
    result
  end

  def full_subst_eliminate_all_variables() do
    tree = btvp(btvv(1), btvv(2))

    subst = fn
      1 -> btva(:x)
      2 -> btva(:y)
    end

    result = BinTree.full_subst(subst, tree)
    expected = btvp(btva(:x), btva(:y))
    assert result == expected
    result
  end

  def full_subst_with_complex_trees() do
    tree = btvp(btvv(:a), btvv(:b))

    subst = fn
      :a -> btvp(btva(1), btva(2))
      :b -> btva(3)
    end

    result = BinTree.full_subst(subst, tree)
    expected = btvp(btvp(btva(1), btva(2)), btva(3))
    assert result == expected
    result
  end

  def depth_of_atom() do
    tree = btva(:foo)
    result = BinTree.depth(tree)
    assert result == 1
    result
  end

  def depth_of_variable() do
    tree = btvv(42)
    result = BinTree.depth(tree)
    assert result == 0
    result
  end

  def depth_of_pair() do
    tree = btvp(btva(:a), btva(:b))
    result = BinTree.depth(tree)
    assert result == 2
    result
  end

  def depth_of_nested_pair() do
    tree = btvp(btvp(btva(:a), btva(:b)), btva(:c))
    result = BinTree.depth(tree)
    assert result == 3
    result
  end

  def depth_of_unbalanced_tree() do
    tree = btvp(btva(:a), btvp(btva(:b), btvp(btva(:c), btva(:d))))
    result = BinTree.depth(tree)
    assert result == 4
    result
  end

  def size_of_atom() do
    tree = btva(:foo)
    result = BinTree.size(tree)
    assert result == 1
    result
  end

  def size_of_variable() do
    tree = btvv(42)
    result = BinTree.size(tree)
    assert result == 0
    result
  end

  def size_of_pair() do
    tree = btvp(btva(:a), btva(:b))
    result = BinTree.size(tree)
    assert result == 2
    result
  end

  def size_of_nested_pair() do
    tree = btvp(btvp(btva(:a), btva(:b)), btva(:c))
    result = BinTree.size(tree)
    assert result == 3
    result
  end

  def size_with_variables() do
    tree = btvp(btvv(1), btvp(btva(:x), btvv(2)))
    result = BinTree.size(tree)
    assert result == 1
    result
  end

  def size_of_complex_tree() do
    tree =
      btvp(
        btvp(btva(:a), btva(:b)),
        btvp(btva(:c), btvp(btva(:d), btva(:e)))
      )

    result = BinTree.size(tree)
    assert result == 5
    result
  end
end
