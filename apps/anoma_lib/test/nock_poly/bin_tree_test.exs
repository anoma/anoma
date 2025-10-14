defmodule BinTreeTest do
  use ExUnit.Case, async: true

  import NockPoly.BinTree.MacroDefs

  alias NockPoly.BinTree

  doctest NockPoly.BinTree.MacroDefs

  describe "basic construction" do
    test "atom term" do
      tree = btva(:foo)
      assert tree == {:in_btv, {:btatom, :foo}}
      tree
    end

    test "variable term" do
      tree = btvv(42)
      assert tree == {:in_btv, {:btvar, 42}}
      tree
    end

    test "pair term" do
      tree = btvp(btva(:a), btva(:b))

      assert tree ==
               {:in_btv,
                {:btpair, {:in_btv, {:btatom, :a}}, {:in_btv, {:btatom, :b}}}}

      tree
    end

    test "nested pair" do
      tree = btvp(btvp(btva(:a), btva(:b)), btva(:c))

      assert tree ==
               {:in_btv,
                {:btpair,
                 {:in_btv,
                  {:btpair, {:in_btv, {:btatom, :a}},
                   {:in_btv, {:btatom, :b}}}}, {:in_btv, {:btatom, :c}}}}

      tree
    end
  end

  describe "functor bintreef" do
    test "map over atom" do
      tree_f = {:atom, :foo}
      result = BinTree.bintreef_map(&String.to_atom("mapped_#{&1}"), tree_f)
      assert result == {:atom, :foo}
      result
    end

    test "map over pair" do
      tree_f = {:pair, 1, 2}
      result = BinTree.bintreef_map(&(&1 * 10), tree_f)
      assert result == {:pair, 10, 20}
      result
    end
  end

  describe "functor bintreefv" do
    test "bimap over variable" do
      tree_fv = {:btvar, 42}
      result = BinTree.bintreefv_bimap(&(&1 + 1), &(&1 * 10), tree_fv)
      assert result == {:btvar, 43}
      result
    end

    test "bimap over atom" do
      tree_fv = {:btatom, :foo}

      result =
        BinTree.bintreefv_bimap(&(&1 + 1), &(&1 * 10), tree_fv)

      assert result == {:btatom, :foo}
      result
    end

    test "bimap over pair" do
      tree_fv = {:btpair, 1, 2}
      result = BinTree.bintreefv_bimap(&(&1 + 1), &(&1 * 10), tree_fv)
      assert result == {:btpair, 10, 20}
      result
    end
  end

  describe "algebra and coalgebra" do
    test "in_btv and out_btv are inverses" do
      tree_fv = {:btatom, :test}
      tree = BinTree.in_btv(tree_fv)
      assert BinTree.out_btv(tree) == tree_fv
      tree
    end
  end

  describe "eval" do
    test "eval on atom tree" do
      tree = btva(:foo)

      algebra = fn
        {:atom, a} -> "atom: #{a}"
        {:pair, l, r} -> "pair: #{l}, #{r}"
      end

      subst = fn v -> "var: #{v}" end

      result = BinTree.eval(algebra, subst, tree)
      assert result == "atom: foo"
      result
    end

    test "eval on variable tree" do
      tree = btvv(42)

      algebra = fn
        {:atom, a} -> "atom: #{a}"
        {:pair, l, r} -> "pair: #{l}, #{r}"
      end

      subst = fn v -> "var: #{v}" end

      result = BinTree.eval(algebra, subst, tree)
      assert result == "var: 42"
      result
    end

    test "eval on pair tree" do
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

    test "eval on nested pair tree" do
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

    test "eval with variable substitution" do
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
  end

  describe "cata" do
    test "cata on atom tree" do
      tree = btva(:test)

      algebra = fn
        {:atom, a} -> {:leaf, a}
        {:pair, l, r} -> {:node, l, r}
      end

      result = BinTree.cata(tree, algebra)
      assert result == {:leaf, :test}
      result
    end

    test "cata on pair tree" do
      tree = btvp(btva(:a), btva(:b))

      algebra = fn
        {:atom, a} -> {:leaf, a}
        {:pair, l, r} -> {:node, l, r}
      end

      result = BinTree.cata(tree, algebra)
      assert result == {:node, {:leaf, :a}, {:leaf, :b}}
      result
    end

    test "cata sum of integer atoms" do
      tree = btvp(btvp(btva(1), btva(2)), btva(3))

      algebra = fn
        {:atom, a} -> a
        {:pair, l, r} -> l + r
      end

      result = BinTree.cata(tree, algebra)
      assert result == 6
      result
    end
  end

  describe "prod_eval_mon" do
    test "evaluate with product algebra on simple tree" do
      tree = btvp(btva(10), btva(20))
      prod_alg = fn {l, r} -> l + r end

      result = BinTree.prod_eval_mon(prod_alg, tree)
      assert result == 30
      result
    end

    test "evaluate with product algebra on nested tree" do
      tree = btvp(btvp(btva(1), btva(2)), btva(3))
      prod_alg = fn {l, r} -> l + r end

      result = BinTree.prod_eval_mon(prod_alg, tree)
      assert result == 6
      result
    end

    test "evaluate with product algebra concatenation" do
      tree = btvp(btvp(btva("a"), btva("b")), btva("c"))
      prod_alg = fn {l, r} -> l <> r end

      result = BinTree.prod_eval_mon(prod_alg, tree)
      assert result == "abc"
      result
    end
  end

  describe "btamap" do
    test "map over atoms in tree" do
      tree = btvp(btva(:foo), btva(:bar))
      result = BinTree.btamap(&String.to_atom("x_#{&1}"), tree)

      expected = btvp(btva(:x_foo), btva(:x_bar))
      assert result == expected
      result
    end

    test "map preserves structure" do
      tree = btvp(btvp(btva(1), btva(2)), btva(3))
      result = BinTree.btamap(&(&1 * 10), tree)

      expected = btvp(btvp(btva(10), btva(20)), btva(30))
      assert result == expected
      result
    end
  end

  describe "btvmap" do
    test "map over variables in tree" do
      tree = btvp(btvv(1), btvv(2))
      result = BinTree.btvmap(&(&1 + 10), tree)

      expected = btvp(btvv(11), btvv(12))
      assert result == expected
      result
    end

    test "map over variables preserves atoms" do
      tree = btvp(btvv(1), btva(:foo))
      result = BinTree.btvmap(&(&1 * 2), tree)

      expected = btvp(btvv(2), btva(:foo))
      assert result == expected
      result
    end

    test "map over variables in nested tree" do
      tree = btvp(btvp(btvv(1), btvv(2)), btvv(3))
      result = BinTree.btvmap(&(&1 + 100), tree)

      expected = btvp(btvp(btvv(101), btvv(102)), btvv(103))
      assert result == expected
      result
    end
  end

  describe "comultiplication and join" do
    test "btv_comult creates nested structure" do
      tree = btvp(btva(:a), btvv(1))
      result = BinTree.btv_comult(tree)

      expected = btvp(btva(:a), btvv(btvv(1)))
      assert result == expected
      result
    end

    test "btv_mult flattens nested structure" do
      tree = btvp(btva(:a), btvv(btvv(1)))
      result = BinTree.btv_mult(tree)

      expected = btvp(btva(:a), btvv(1))
      assert result == expected
      result
    end

    test "mult after comult is identity" do
      tree = btvp(btvp(btva(:a), btva(:b)), btvv(1))
      result = BinTree.btv_mult(BinTree.btv_comult(tree))
      assert result == tree
      result
    end
  end

  describe "bind" do
    test "bind substitutes variables with trees" do
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

    test "bind with complex substitution" do
      tree = btvp(btvv(1), btva(:x))

      subst = fn 1 -> btvp(btva(:a), btva(:b)) end

      result = BinTree.btv_bind(subst, tree)
      expected = btvp(btvp(btva(:a), btva(:b)), btva(:x))
      assert result == expected
      result
    end

    test "bind with nested tree" do
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
  end

  describe "full_subst" do
    test "eliminate all variables" do
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

    test "full substitution with complex trees" do
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
  end

  describe "depth" do
    test "depth of atom" do
      tree = btva(:foo)
      assert BinTree.depth(tree) == 1
    end

    test "depth of variable" do
      tree = btvv(42)
      assert BinTree.depth(tree) == 0
    end

    test "depth of pair" do
      tree = btvp(btva(:a), btva(:b))
      assert BinTree.depth(tree) == 2
    end

    test "depth of nested pair" do
      tree = btvp(btvp(btva(:a), btva(:b)), btva(:c))
      assert BinTree.depth(tree) == 3
    end

    test "depth of unbalanced tree" do
      tree = btvp(btva(:a), btvp(btva(:b), btvp(btva(:c), btva(:d))))
      assert BinTree.depth(tree) == 4
    end
  end

  describe "size" do
    test "size of atom" do
      tree = btva(:foo)
      assert BinTree.size(tree) == 1
    end

    test "size of variable" do
      tree = btvv(42)
      assert BinTree.size(tree) == 0
    end

    test "size of pair" do
      tree = btvp(btva(:a), btva(:b))
      assert BinTree.size(tree) == 2
    end

    test "size of nested pair" do
      tree = btvp(btvp(btva(:a), btva(:b)), btva(:c))
      assert BinTree.size(tree) == 3
    end

    test "size with variables" do
      tree = btvp(btvv(1), btvp(btva(:x), btvv(2)))
      assert BinTree.size(tree) == 1
    end

    test "size of complex tree" do
      tree =
        btvp(
          btvp(btva(:a), btva(:b)),
          btvp(btva(:c), btvp(btva(:d), btva(:e)))
        )

      assert BinTree.size(tree) == 5
    end
  end
end
