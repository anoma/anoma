defmodule Examples.ENockPoly.EBinTreeTerm do
  use Memoize

  import ExUnit.Assertions
  import NockPoly.BinTree.MacroDefs
  import NockPoly.Term.MacroDefs

  alias NockPoly.BinTreeTerm

  def term_to_bintree_nullary() do
    term = tvc0(:foo)
    tree = BinTreeTerm.term_to_bintree(term)
    assert tree == btva(:foo)
    tree
  end

  def term_to_bintree_unary() do
    term = tvc(:foo, [tvc0(:bar)])
    tree = BinTreeTerm.term_to_bintree(term)
    expected = btvp(btva(:foo), btva(:bar))
    assert tree == expected
    tree
  end

  def term_to_bintree_binary() do
    term = tvc(:foo, [tvc0(:a), tvc0(:b)])
    tree = BinTreeTerm.term_to_bintree(term)
    expected = btvp(btvp(btva(:foo), btva(:b)), btva(:a))
    assert tree == expected
    tree
  end

  def term_to_bintree_ternary() do
    term = tvc(:f, [tvc0(:x), tvc0(:y), tvc0(:z)])
    tree = BinTreeTerm.term_to_bintree(term)
    expected = btvp(btvp(btvp(btva(:f), btva(:z)), btva(:y)), btva(:x))
    assert tree == expected
    tree
  end

  def bintree_to_term_single_atom() do
    tree = btva(:foo)
    term = BinTreeTerm.bintree_to_term(tree)
    assert term == tvc0(:foo)
    term
  end

  def bintree_to_term_pair_with_atom_leaves() do
    tree = btvp(btva(:foo), btva(:bar))
    term = BinTreeTerm.bintree_to_term(tree)
    expected = tvc(:foo, [tvc0(:bar)])
    assert term == expected
    term
  end

  def bintree_to_term_nested_pair() do
    tree = btvp(btvp(btva(:foo), btva(:bar)), btva(:baz))
    term = BinTreeTerm.bintree_to_term(tree)
    expected = tvc(:foo, [tvc0(:baz), tvc0(:bar)])
    assert term == expected
    term
  end

  def bintree_to_term_triple_nested_pair() do
    tree = btvp(btvp(btvp(btva(:f), btva(:x)), btva(:y)), btva(:z))
    term = BinTreeTerm.bintree_to_term(tree)
    expected = tvc(:f, [tvc0(:z), tvc0(:y), tvc0(:x)])
    assert term == expected
    term
  end

  def term_to_bintree_nested_binary_constructors() do
    term =
      tvc(:foo, [
        tvc(:bar, [tvc0(:a), tvc0(:b)]),
        tvc(:baz, [tvc0(:c), tvc0(:d)])
      ])

    tree = BinTreeTerm.term_to_bintree(term)

    bar_tree = btvp(btvp(btva(:bar), btva(:b)), btva(:a))
    baz_tree = btvp(btvp(btva(:baz), btva(:d)), btva(:c))
    expected = btvp(btvp(btva(:foo), baz_tree), bar_tree)

    assert tree == expected
    tree
  end

  def term_to_bintree_deeply_nested_unary() do
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

  def term_to_bintree_mixed_arity() do
    term =
      tvc(:root, [
        tvc0(:a),
        tvc(:b, [tvc0(:x)]),
        tvc(:c, [tvc0(:y), tvc0(:z)])
      ])

    tree = BinTreeTerm.term_to_bintree(term)

    b_tree = btvp(btva(:b), btva(:x))
    c_tree = btvp(btvp(btva(:c), btva(:z)), btva(:y))

    expected =
      btvp(
        btvp(btvp(btva(:root), c_tree), b_tree),
        btva(:a)
      )

    assert tree == expected
    tree
  end

  def term_to_bintree_list_like() do
    term =
      tvc(:cons, [
        tvc0(1),
        tvc(:cons, [
          tvc0(2),
          tvc(:cons, [tvc0(3), tvc0(nil)])
        ])
      ])

    tree = BinTreeTerm.term_to_bintree(term)

    inner_cons = btvp(btvp(btva(:cons), btva(nil)), btva(3))
    mid_cons = btvp(btvp(btva(:cons), inner_cons), btva(2))
    expected = btvp(btvp(btva(:cons), mid_cons), btva(1))

    assert tree == expected
    tree
  end

  def bintree_to_term_complex_nested_pairs() do
    tree =
      btvp(
        btvp(btvp(btva(:a), btva(:b)), btva(:c)),
        btva(:d)
      )

    term = BinTreeTerm.bintree_to_term(tree)
    expected = tvc(:a, [tvc0(:d), tvc0(:c), tvc0(:b)])
    assert term == expected
    term
  end

  def bintree_to_term_unbalanced_tree() do
    tree =
      btvp(
        btva(:a),
        btvp(
          btva(:b),
          btvp(btva(:c), btva(:d))
        )
      )

    term = BinTreeTerm.bintree_to_term(tree)

    expected =
      tvc(:a, [
        tvc(:b, [
          tvc(:c, [tvc0(:d)])
        ])
      ])

    assert term == expected
    term
  end

  def roundtrip_term_nullary() do
    term = tvc0(:atom)
    result = BinTreeTerm.roundtrip_term(term)
    assert result == term
    result
  end

  def roundtrip_term_unary() do
    term = tvc(:f, [tvc0(:x)])
    result = BinTreeTerm.roundtrip_term(term)
    assert result == term
    result
  end

  def roundtrip_term_binary() do
    term = tvc(:f, [tvc0(:x), tvc0(:y)])
    result = BinTreeTerm.roundtrip_term(term)
    assert result == term
    result
  end

  def roundtrip_term_ternary() do
    term = tvc(:f, [tvc0(:a), tvc0(:b), tvc0(:c)])
    result = BinTreeTerm.roundtrip_term(term)
    assert result == term
    result
  end

  def roundtrip_term_quaternary() do
    term = tvc(:f, [tvc0(1), tvc0(2), tvc0(3), tvc0(4)])
    result = BinTreeTerm.roundtrip_term(term)
    assert result == term
    result
  end

  def roundtrip_term_nested_binary() do
    term =
      tvc(:foo, [
        tvc(:bar, [tvc0(:a), tvc0(:b)]),
        tvc(:baz, [tvc0(:c), tvc0(:d)])
      ])

    result = BinTreeTerm.roundtrip_term(term)
    assert result == term
    result
  end

  def roundtrip_term_deeply_nested_unary() do
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

  def roundtrip_term_mixed_arity() do
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

  def roundtrip_term_list_like() do
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

  def roundtrip_term_tree_like_multilevel() do
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

  def roundtrip_bintree_single_atom() do
    tree = btva(:x)
    result = BinTreeTerm.roundtrip_bintree(tree)
    assert result == tree
    result
  end

  def roundtrip_bintree_simple_pair() do
    tree = btvp(btva(:a), btva(:b))
    result = BinTreeTerm.roundtrip_bintree(tree)
    assert result == tree
    result
  end

  def roundtrip_bintree_nested_pair() do
    tree = btvp(btvp(btva(:a), btva(:b)), btva(:c))
    result = BinTreeTerm.roundtrip_bintree(tree)
    assert result == tree
    result
  end

  def roundtrip_bintree_deeply_nested_left_assoc() do
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

  def roundtrip_bintree_right_assoc() do
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

  def roundtrip_bintree_balanced() do
    tree =
      btvp(
        btvp(btva(:a), btva(:b)),
        btvp(btva(:c), btva(:d))
      )

    result = BinTreeTerm.roundtrip_bintree(tree)
    assert result == tree
    result
  end

  def roundtrip_bintree_complex_nested() do
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

  def termv_to_bintreev_with_variables() do
    term = tvc(:f, [tvv(1), tvc0(:x), tvv(2)])
    tree = BinTreeTerm.termv_to_bintreev(term)

    expected =
      btvp(
        btvp(btvp(btva(:f), btvv(2)), btva(:x)),
        btvv(1)
      )

    assert tree == expected
    tree
  end

  def bintreev_to_termv_with_variables() do
    tree = btvp(btvp(btva(:f), btvv(1)), btva(:x))
    term = BinTreeTerm.bintreev_to_termv(tree)

    expected = tvc(:f, [tvc0(:x), tvv(1)])
    assert term == expected
    term
  end

  def roundtrip_termv_with_variables() do
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

  def roundtrip_bintreev_with_variables() do
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

  def roundtrip_term_multiple_times() do
    term = tvc(:f, [tvc(:g, [tvc0(:a), tvc0(:b)]), tvc0(:c)])

    r1 = BinTreeTerm.roundtrip_term(term)
    r2 = BinTreeTerm.roundtrip_term(r1)
    r3 = BinTreeTerm.roundtrip_term(r2)

    assert r1 == term
    assert r2 == term
    assert r3 == term
    r3
  end

  def roundtrip_bintree_multiple_times() do
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

  def alternating_roundtrips_for_terms() do
    term = tvc(:root, [tvc0(1), tvc0(2), tvc0(3)])

    t1 = BinTreeTerm.term_to_bintree(term)
    term1 = BinTreeTerm.bintree_to_term(t1)
    t2 = BinTreeTerm.term_to_bintree(term1)
    term2 = BinTreeTerm.bintree_to_term(t2)

    assert term1 == term
    assert t2 == t1
    assert term2 == term
    term2
  end

  def alternating_roundtrips_for_trees() do
    tree = btvp(btvp(btvp(btva(:w), btva(:x)), btva(:y)), btva(:z))

    term1 = BinTreeTerm.bintree_to_term(tree)
    t1 = BinTreeTerm.term_to_bintree(term1)
    term2 = BinTreeTerm.bintree_to_term(t1)
    t2 = BinTreeTerm.term_to_bintree(term2)

    assert term2 == term1
    assert t1 == tree
    assert t2 == tree
    t2
  end

  def interpretable_valid_tree_with_variable_in_right_position() do
    tree = btvp(btva(:foo), btvv(:x))
    result = BinTreeTerm.bintreev_interpretable_as_termv?(tree)
    assert result == true
    result
  end

  def interpretable_valid_tree_nullary() do
    tree = btva(:foo)
    result = BinTreeTerm.bintreev_interpretable_as_termv?(tree)
    assert result == true
    result
  end

  def interpretable_invalid_tree_variable_in_left_position() do
    tree = btvp(btvv(:x), btva(:foo))
    result = BinTreeTerm.bintreev_interpretable_as_termv?(tree)
    assert result == false
    result
  end

  def interpretable_invalid_tree_variable_in_nested_left_position() do
    tree = btvp(btvp(btvv(:x), btva(:bar)), btva(:foo))
    result = BinTreeTerm.bintreev_interpretable_as_termv?(tree)
    assert result == false
    result
  end

  def roundtrip_termv_to_bintreev_always_interpretable() do
    term = tvc(:f, [tvv(:x), tvc(:g, [tvv(:y)]), tvc0(:z)])
    tree = BinTreeTerm.termv_to_bintreev(term)
    result = BinTreeTerm.bintreev_interpretable_as_termv?(tree)
    assert result == true

    converted_term = BinTreeTerm.bintreev_to_termv(tree)
    assert converted_term == term
    result
  end

  def bintreev_to_termv_raises_on_variable_in_left_position() do
    tree = btvp(btvv(:x), btva(:foo))

    assert_raise ArgumentError, fn ->
      BinTreeTerm.bintreev_to_termv(tree)
    end

    tree
  end

  def bintreev_to_termv_raises_on_nested_variable_in_left_position() do
    tree = btvp(btvp(btva(:f), btva(:g)), btvp(btvv(:x), btva(:h)))

    assert_raise ArgumentError, fn ->
      BinTreeTerm.bintreev_to_termv(tree)
    end

    tree
  end

  def term_to_bintree_right_nullary() do
    term = tvc0(:foo)
    tree = BinTreeTerm.term_to_bintree_right(term)
    assert tree == btva(:foo)
    tree
  end

  def term_to_bintree_right_unary() do
    term = tvc(:foo, [tvc0(:bar)])
    tree = BinTreeTerm.term_to_bintree_right(term)
    expected = btvp(btva(:foo), btva(:bar))
    assert tree == expected
    tree
  end

  def term_to_bintree_right_binary() do
    term = tvc(:foo, [tvc0(:a), tvc0(:b)])
    tree = BinTreeTerm.term_to_bintree_right(term)
    expected = btvp(btva(:foo), btvp(btva(:a), btva(:b)))
    assert tree == expected
    tree
  end

  def term_to_bintree_right_ternary() do
    term = tvc(:f, [tvc0(:x), tvc0(:y), tvc0(:z)])
    tree = BinTreeTerm.term_to_bintree_right(term)
    expected = btvp(btva(:f), btvp(btva(:x), btvp(btva(:y), btva(:z))))
    assert tree == expected
    tree
  end

  def term_to_bintree_right_nested() do
    term = tvc(:f, [tvc(:g, [tvc0(:a), tvc0(:b)]), tvc0(:c)])
    tree = BinTreeTerm.term_to_bintree_right(term)

    expected =
      btvp(
        btva(:f),
        btvp(
          btvp(btva(:g), btvp(btva(:a), btva(:b))),
          btva(:c)
        )
      )

    assert tree == expected
    tree
  end

  def termv_to_bintreev_right_with_variables() do
    term = tvc(:f, [tvv(:x), tvc(:g, [tvv(:y)]), tvc0(:z)])
    tree = BinTreeTerm.termv_to_bintreev_right(term)

    expected =
      btvp(
        btva(:f),
        btvp(
          btvv(:x),
          btvp(
            btvp(btva(:g), btvv(:y)),
            btva(:z)
          )
        )
      )

    assert tree == expected
    tree
  end
end
