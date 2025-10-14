defmodule BinTreeTermTest do
  use ExUnit.Case, async: true

  import NockPoly.BinTree.MacroDefs
  import NockPoly.Term.MacroDefs

  alias NockPoly.BinTreeTerm

  describe "term to bintree - simple cases" do
    test "nullary constructor (no children)" do
      term = tvc0(:foo)
      tree = BinTreeTerm.term_to_bintree(term)
      assert tree == btva(:foo)
      tree
    end

    test "unary constructor" do
      term = tvc(:foo, [tvc0(:bar)])
      tree = BinTreeTerm.term_to_bintree(term)
      # Snoclist [bar] becomes (foo . bar)
      expected = btvp(btva(:foo), btva(:bar))
      assert tree == expected
      tree
    end

    test "binary constructor" do
      term = tvc(:foo, [tvc0(:a), tvc0(:b)])
      tree = BinTreeTerm.term_to_bintree(term)
      # Snoclist [a, b] becomes ((foo . b) . a)
      expected = btvp(btvp(btva(:foo), btva(:b)), btva(:a))
      assert tree == expected
      tree
    end

    test "ternary constructor" do
      term = tvc(:f, [tvc0(:x), tvc0(:y), tvc0(:z)])
      tree = BinTreeTerm.term_to_bintree(term)
      # Snoclist [x, y, z] becomes (((f . z) . y) . x)
      expected =
        btvp(btvp(btvp(btva(:f), btva(:z)), btva(:y)), btva(:x))

      assert tree == expected
      tree
    end
  end

  describe "bintree to term - simple cases" do
    test "single atom" do
      tree = btva(:foo)
      term = BinTreeTerm.bintree_to_term(tree)
      assert term == tvc0(:foo)
      term
    end

    test "pair with atom leaves" do
      tree = btvp(btva(:foo), btva(:bar))
      term = BinTreeTerm.bintree_to_term(tree)
      # (foo . bar) becomes foo with child [bar]
      expected = tvc(:foo, [tvc0(:bar)])
      assert term == expected
      term
    end

    test "nested pair" do
      tree = btvp(btvp(btva(:foo), btva(:bar)), btva(:baz))
      term = BinTreeTerm.bintree_to_term(tree)
      # ((foo . bar) . baz) becomes foo with children [baz, bar]
      expected = tvc(:foo, [tvc0(:baz), tvc0(:bar)])
      assert term == expected
      term
    end

    test "triple nested pair" do
      tree =
        btvp(btvp(btvp(btva(:f), btva(:x)), btva(:y)), btva(:z))

      term = BinTreeTerm.bintree_to_term(tree)
      # (((f . x) . y) . z) becomes f with children [z, y, x]
      expected = tvc(:f, [tvc0(:z), tvc0(:y), tvc0(:x)])
      assert term == expected
      term
    end
  end

  describe "multi-level structures - term to bintree" do
    test "nested binary constructors" do
      # foo(bar(a, b), baz(c, d))
      term =
        tvc(:foo, [
          tvc(:bar, [tvc0(:a), tvc0(:b)]),
          tvc(:baz, [tvc0(:c), tvc0(:d)])
        ])

      tree = BinTreeTerm.term_to_bintree(term)

      # bar(a, b) => ((bar . b) . a)
      bar_tree = btvp(btvp(btva(:bar), btva(:b)), btva(:a))
      # baz(c, d) => ((baz . d) . c)
      baz_tree = btvp(btvp(btva(:baz), btva(:d)), btva(:c))
      # foo with children [bar(...), baz(...)] as snoclist
      # => ((foo . baz(...)) . bar(...))
      expected = btvp(btvp(btva(:foo), baz_tree), bar_tree)

      assert tree == expected
      tree
    end

    test "deeply nested unary constructors" do
      # a(b(c(d(e))))
      term =
        tvc(:a, [
          tvc(:b, [
            tvc(:c, [
              tvc(:d, [
                tvc0(:e)
              ])
            ])
          ])
        ])

      tree = BinTreeTerm.term_to_bintree(term)

      # Build from inside out:
      # e => e
      # d(e) => (d . e)
      # c(d(e)) => (c . (d . e))
      # b(c(d(e))) => (b . (c . (d . e)))
      # a(b(c(d(e)))) => (a . (b . (c . (d . e))))
      expected =
        btvp(
          btva(:a),
          btvp(
            btva(:b),
            btvp(
              btva(:c),
              btvp(btva(:d), btva(:e))
            )
          )
        )

      assert tree == expected
      tree
    end

    test "mixed arity constructors" do
      # root(a, b(x), c(y, z))
      term =
        tvc(:root, [
          tvc0(:a),
          tvc(:b, [tvc0(:x)]),
          tvc(:c, [tvc0(:y), tvc0(:z)])
        ])

      tree = BinTreeTerm.term_to_bintree(term)

      # a => a
      # b(x) => (b . x)
      b_tree = btvp(btva(:b), btva(:x))
      # c(y, z) => ((c . z) . y)
      c_tree = btvp(btvp(btva(:c), btva(:z)), btva(:y))
      # root with children [a, b(x), c(y,z)] as snoclist
      # => (((root . c(...)) . b(...)) . a)
      expected =
        btvp(
          btvp(btvp(btva(:root), c_tree), b_tree),
          btva(:a)
        )

      assert tree == expected
      tree
    end

    test "list-like structure" do
      # cons(1, cons(2, cons(3, nil)))
      term =
        tvc(:cons, [
          tvc0(1),
          tvc(:cons, [
            tvc0(2),
            tvc(:cons, [tvc0(3), tvc0(nil)])
          ])
        ])

      tree = BinTreeTerm.term_to_bintree(term)

      # Build inner cons(3, nil) => ((cons . nil) . 3)
      inner_cons = btvp(btvp(btva(:cons), btva(nil)), btva(3))
      # cons(2, inner) => ((cons . inner) . 2)
      mid_cons = btvp(btvp(btva(:cons), inner_cons), btva(2))
      # cons(1, mid) => ((cons . mid) . 1)
      expected = btvp(btvp(btva(:cons), mid_cons), btva(1))

      assert tree == expected
      tree
    end
  end

  describe "multi-level structures - bintree to term" do
    test "complex nested pairs to nested terms" do
      # ((( a . b ) . c) . d)
      tree =
        btvp(
          btvp(btvp(btva(:a), btva(:b)), btva(:c)),
          btva(:d)
        )

      term = BinTreeTerm.bintree_to_term(tree)
      # Should become a with children [d, c, b]
      expected = tvc(:a, [tvc0(:d), tvc0(:c), tvc0(:b)])
      assert term == expected
      term
    end

    test "unbalanced tree structure" do
      # (a . (b . (c . d)))
      tree =
        btvp(
          btva(:a),
          btvp(
            btva(:b),
            btvp(btva(:c), btva(:d))
          )
        )

      term = BinTreeTerm.bintree_to_term(tree)

      # (c . d) => c([d])
      # (b . c([d])) => b([c([d])])
      # (a . b([c([d])])) => a([b([c([d])])])
      expected =
        tvc(:a, [
          tvc(:b, [
            tvc(:c, [tvc0(:d)])
          ])
        ])

      assert term == expected
      term
    end
  end

  describe "roundtrip - term -> bintree -> term" do
    test "nullary constructor roundtrips" do
      term = tvc0(:atom)
      result = BinTreeTerm.roundtrip_term(term)
      assert result == term
      result
    end

    test "unary constructor roundtrips" do
      term = tvc(:f, [tvc0(:x)])
      result = BinTreeTerm.roundtrip_term(term)
      assert result == term
      result
    end

    test "binary constructor roundtrips" do
      term = tvc(:f, [tvc0(:x), tvc0(:y)])
      result = BinTreeTerm.roundtrip_term(term)
      assert result == term
      result
    end

    test "ternary constructor roundtrips" do
      term = tvc(:f, [tvc0(:a), tvc0(:b), tvc0(:c)])
      result = BinTreeTerm.roundtrip_term(term)
      assert result == term
      result
    end

    test "quaternary constructor roundtrips" do
      term = tvc(:f, [tvc0(1), tvc0(2), tvc0(3), tvc0(4)])
      result = BinTreeTerm.roundtrip_term(term)
      assert result == term
      result
    end

    test "nested binary constructors roundtrip" do
      term =
        tvc(:foo, [
          tvc(:bar, [tvc0(:a), tvc0(:b)]),
          tvc(:baz, [tvc0(:c), tvc0(:d)])
        ])

      result = BinTreeTerm.roundtrip_term(term)
      assert result == term
      result
    end

    test "deeply nested unary constructors roundtrip" do
      term =
        tvc(:a, [
          tvc(:b, [
            tvc(:c, [
              tvc(:d, [
                tvc(:e, [
                  tvc0(:f)
                ])
              ])
            ])
          ])
        ])

      result = BinTreeTerm.roundtrip_term(term)
      assert result == term
      result
    end

    test "mixed arity constructors roundtrip" do
      term =
        tvc(:root, [
          tvc0(:a),
          tvc(:b, [tvc0(:x)]),
          tvc(:c, [tvc0(:y), tvc0(:z)]),
          tvc(:d, [tvc0(1), tvc0(2), tvc0(3)])
        ])

      result = BinTreeTerm.roundtrip_term(term)
      assert result == term
      result
    end

    test "list-like structure roundtrips" do
      term =
        tvc(:cons, [
          tvc0(1),
          tvc(:cons, [
            tvc0(2),
            tvc(:cons, [
              tvc0(3),
              tvc(:cons, [
                tvc0(4),
                tvc0(nil)
              ])
            ])
          ])
        ])

      result = BinTreeTerm.roundtrip_term(term)
      assert result == term
      result
    end

    test "tree-like structure with multiple levels roundtrips" do
      term =
        tvc(:root, [
          tvc(:left, [
            tvc(:ll, [tvc0(:a), tvc0(:b)]),
            tvc(:lr, [tvc0(:c)])
          ]),
          tvc(:right, [
            tvc(:rl, [tvc0(:d)]),
            tvc(:rr, [tvc0(:e), tvc0(:f)])
          ])
        ])

      result = BinTreeTerm.roundtrip_term(term)
      assert result == term
      result
    end
  end

  describe "roundtrip - bintree -> term -> bintree" do
    test "single atom roundtrips" do
      tree = btva(:x)
      result = BinTreeTerm.roundtrip_bintree(tree)
      assert result == tree
      result
    end

    test "simple pair roundtrips" do
      tree = btvp(btva(:a), btva(:b))
      result = BinTreeTerm.roundtrip_bintree(tree)
      assert result == tree
      result
    end

    test "nested pair roundtrips" do
      tree = btvp(btvp(btva(:a), btva(:b)), btva(:c))
      result = BinTreeTerm.roundtrip_bintree(tree)
      assert result == tree
      result
    end

    test "deeply nested left-associative pairs roundtrip" do
      tree =
        btvp(
          btvp(
            btvp(
              btvp(btva(:a), btva(:b)),
              btva(:c)
            ),
            btva(:d)
          ),
          btva(:e)
        )

      result = BinTreeTerm.roundtrip_bintree(tree)
      assert result == tree
      result
    end

    test "right-associative pairs roundtrip" do
      tree =
        btvp(
          btva(:a),
          btvp(
            btva(:b),
            btvp(
              btva(:c),
              btvp(btva(:d), btva(:e))
            )
          )
        )

      result = BinTreeTerm.roundtrip_bintree(tree)
      assert result == tree
      result
    end

    test "balanced tree roundtrips" do
      tree =
        btvp(
          btvp(btva(:a), btva(:b)),
          btvp(btva(:c), btva(:d))
        )

      result = BinTreeTerm.roundtrip_bintree(tree)
      assert result == tree
      result
    end

    test "complex nested structure roundtrips" do
      tree =
        btvp(
          btvp(
            btvp(btva(:a), btva(:b)),
            btvp(btva(:c), btva(:d))
          ),
          btvp(
            btvp(btva(:e), btva(:f)),
            btva(:g)
          )
        )

      result = BinTreeTerm.roundtrip_bintree(tree)
      assert result == tree
      result
    end
  end

  describe "open terms and trees with variables" do
    test "term with variables converts to tree with variables" do
      term = tvc(:f, [tvv(1), tvc0(:x), tvv(2)])
      tree = BinTreeTerm.termv_to_bintreev(term)

      # f with children [var(1), x, var(2)] as snoclist
      # => (((f . var(2)) . x) . var(1))
      expected =
        btvp(
          btvp(btvp(btva(:f), btvv(2)), btva(:x)),
          btvv(1)
        )

      assert tree == expected
      tree
    end

    test "tree with variables converts to term with variables" do
      tree = btvp(btvp(btva(:f), btvv(1)), btva(:x))
      term = BinTreeTerm.bintreev_to_termv(tree)

      # ((f . var(1)) . x) => f with children [x, var(1)]
      expected = tvc(:f, [tvc0(:x), tvv(1)])
      assert term == expected
      term
    end

    test "open term roundtrips through tree" do
      term =
        tvc(:foo, [
          tvv(:x),
          tvc(:bar, [tvv(:y), tvc0(:z)]),
          tvv(:w)
        ])

      result = BinTreeTerm.roundtrip_termv(term)
      assert result == term
      result
    end

    test "open tree roundtrips through term" do
      tree =
        btvp(
          btvp(
            btvp(btva(:a), btvv(1)),
            btvp(btva(:b), btvv(2))
          ),
          btvv(3)
        )

      result = BinTreeTerm.roundtrip_bintreev(tree)
      assert result == tree
      result
    end
  end

  describe "property: roundtrips are identity" do
    test "multiple term roundtrips reach fixpoint" do
      term = tvc(:f, [tvc(:g, [tvc0(:a), tvc0(:b)]), tvc0(:c)])

      r1 = BinTreeTerm.roundtrip_term(term)
      r2 = BinTreeTerm.roundtrip_term(r1)
      r3 = BinTreeTerm.roundtrip_term(r2)

      assert r1 == term
      assert r2 == term
      assert r3 == term
      r3
    end

    test "multiple bintree roundtrips reach fixpoint" do
      tree =
        btvp(
          btvp(btva(:a), btva(:b)),
          btvp(btva(:c), btva(:d))
        )

      r1 = BinTreeTerm.roundtrip_bintree(tree)
      r2 = BinTreeTerm.roundtrip_bintree(r1)
      r3 = BinTreeTerm.roundtrip_bintree(r2)

      assert r1 == tree
      assert r2 == tree
      assert r3 == tree
      r3
    end

    test "alternating roundtrips for terms" do
      term = tvc(:root, [tvc0(1), tvc0(2), tvc0(3)])

      # term -> tree -> term -> tree -> term
      t1 = BinTreeTerm.term_to_bintree(term)
      term1 = BinTreeTerm.bintree_to_term(t1)
      t2 = BinTreeTerm.term_to_bintree(term1)
      term2 = BinTreeTerm.bintree_to_term(t2)

      assert term1 == term
      assert t2 == t1
      assert term2 == term
      term2
    end

    test "alternating roundtrips for trees" do
      tree = btvp(btvp(btvp(btva(:w), btva(:x)), btva(:y)), btva(:z))

      # tree -> term -> tree -> term -> tree
      term1 = BinTreeTerm.bintree_to_term(tree)
      t1 = BinTreeTerm.term_to_bintree(term1)
      term2 = BinTreeTerm.bintree_to_term(t1)
      t2 = BinTreeTerm.term_to_bintree(term2)

      assert term2 == term1
      assert t1 == tree
      assert t2 == tree
      t2
    end
  end

  describe "edge cases - malformed trees" do
    test "tree with variable in left position of pair" do
      # This is a malformed tree that cannot come from term_to_bintree
      # but can be constructed directly: (var(x) . atom)
      tree = btvp(btvv(:x), btva(:foo))
      term = BinTreeTerm.bintreev_to_termv(tree)

      # The function returns the variable as-is in this degenerate case
      # This exercises the {:tvar, _v} -> left_term branch
      assert term == tvv(:x)
      term
    end
  end
end
