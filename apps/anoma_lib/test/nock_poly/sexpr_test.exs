defmodule SexprTest do
  use ExUnit.Case, async: true

  import NockPoly.Sexpr.MacroDefs
  import NockPoly.Term.MacroDefs

  alias NockPoly.Sexpr

  doctest NockPoly.Sexpr.MacroDefs

  describe "basic construction" do
    test "variable S-expression" do
      sexpr = sx_var(:x)
      assert sexpr == {:var, :x}
      sexpr
    end

    test "nullary atom S-expression" do
      sexpr = sx_atom0(:foo)
      assert sexpr == {:atom, :foo, []}
      sexpr
    end

    test "unary atom S-expression" do
      sexpr = sx_atom(:succ, [sx_var(:n)])
      assert sexpr == {:atom, :succ, [{:var, :n}]}
      sexpr
    end

    test "binary atom S-expression" do
      sexpr = sx_atom(:plus, [sx_var(:x), sx_var(:y)])
      assert sexpr == {:atom, :plus, [{:var, :x}, {:var, :y}]}
      sexpr
    end

    test "nested S-expression" do
      sexpr =
        sx_atom(:app, [
          sx_atom(:lambda, [sx_var(:x), sx_var(:body)]),
          sx_atom(:const, [sx_var(:value)])
        ])

      assert sexpr ==
               {:atom, :app,
                [
                  {:atom, :lambda, [{:var, :x}, {:var, :body}]},
                  {:atom, :const, [{:var, :value}]}
                ]}

      sexpr
    end
  end

  describe "to_term" do
    test "variable to term" do
      sexpr = sx_var(:x)
      term = Sexpr.to_term(sexpr)
      assert term == tvv(:x)
      term
    end

    test "nullary atom to term" do
      sexpr = sx_atom0(:foo)
      term = Sexpr.to_term(sexpr)
      assert term == tvc0(:foo)
      term
    end

    test "unary atom to term" do
      sexpr = sx_atom(:f, [sx_var(:x)])
      term = Sexpr.to_term(sexpr)
      assert term == tvc(:f, [tvv(:x)])
      term
    end

    test "binary atom to term" do
      sexpr = sx_atom(:plus, [sx_var(:x), sx_var(:y)])
      term = Sexpr.to_term(sexpr)
      assert term == tvc(:plus, [tvv(:x), tvv(:y)])
      term
    end

    test "nested structure to term" do
      sexpr =
        sx_atom(:f, [
          sx_atom(:g, [sx_atom0(:a), sx_atom0(:b)]),
          sx_atom0(:c)
        ])

      term = Sexpr.to_term(sexpr)

      expected =
        tvc(:f, [
          tvc(:g, [tvc0(:a), tvc0(:b)]),
          tvc0(:c)
        ])

      assert term == expected
      term
    end

    test "deeply nested structure to term" do
      sexpr =
        sx_atom(:root, [
          sx_atom(:left, [
            sx_atom(:ll, [sx_atom0(:a)]),
            sx_atom(:lr, [sx_atom0(:b)])
          ]),
          sx_atom(:right, [
            sx_atom0(:c)
          ])
        ])

      term = Sexpr.to_term(sexpr)

      expected =
        tvc(:root, [
          tvc(:left, [
            tvc(:ll, [tvc0(:a)]),
            tvc(:lr, [tvc0(:b)])
          ]),
          tvc(:right, [
            tvc0(:c)
          ])
        ])

      assert term == expected
      term
    end
  end

  describe "from_term" do
    test "variable from term" do
      term = tvv(:x)
      sexpr = Sexpr.from_term(term)
      assert sexpr == sx_var(:x)
      sexpr
    end

    test "nullary atom from term" do
      term = tvc0(:foo)
      sexpr = Sexpr.from_term(term)
      assert sexpr == sx_atom0(:foo)
      sexpr
    end

    test "unary atom from term" do
      term = tvc(:f, [tvv(:x)])
      sexpr = Sexpr.from_term(term)
      assert sexpr == sx_atom(:f, [sx_var(:x)])
      sexpr
    end

    test "binary atom from term" do
      term = tvc(:plus, [tvv(:x), tvv(:y)])
      sexpr = Sexpr.from_term(term)
      assert sexpr == sx_atom(:plus, [sx_var(:x), sx_var(:y)])
      sexpr
    end

    test "nested structure from term" do
      term =
        tvc(:f, [
          tvc(:g, [tvc0(:a), tvc0(:b)]),
          tvc0(:c)
        ])

      sexpr = Sexpr.from_term(term)

      expected =
        sx_atom(:f, [
          sx_atom(:g, [sx_atom0(:a), sx_atom0(:b)]),
          sx_atom0(:c)
        ])

      assert sexpr == expected
      sexpr
    end

    test "deeply nested structure from term" do
      term =
        tvc(:root, [
          tvc(:left, [
            tvc(:ll, [tvc0(:a)]),
            tvc(:lr, [tvc0(:b)])
          ]),
          tvc(:right, [
            tvc0(:c)
          ])
        ])

      sexpr = Sexpr.from_term(term)

      expected =
        sx_atom(:root, [
          sx_atom(:left, [
            sx_atom(:ll, [sx_atom0(:a)]),
            sx_atom(:lr, [sx_atom0(:b)])
          ]),
          sx_atom(:right, [
            sx_atom0(:c)
          ])
        ])

      assert sexpr == expected
      sexpr
    end
  end

  describe "roundtrip - sexpr -> term -> sexpr" do
    test "variable roundtrips" do
      sexpr = sx_var(:x)
      result = Sexpr.roundtrip_sexpr(sexpr)
      assert result == sexpr
      result
    end

    test "nullary atom roundtrips" do
      sexpr = sx_atom0(:atom)
      result = Sexpr.roundtrip_sexpr(sexpr)
      assert result == sexpr
      result
    end

    test "unary atom roundtrips" do
      sexpr = sx_atom(:f, [sx_var(:x)])
      result = Sexpr.roundtrip_sexpr(sexpr)
      assert result == sexpr
      result
    end

    test "binary atom roundtrips" do
      sexpr = sx_atom(:f, [sx_var(:x), sx_var(:y)])
      result = Sexpr.roundtrip_sexpr(sexpr)
      assert result == sexpr
      result
    end

    test "ternary atom roundtrips" do
      sexpr = sx_atom(:f, [sx_atom0(:a), sx_atom0(:b), sx_atom0(:c)])
      result = Sexpr.roundtrip_sexpr(sexpr)
      assert result == sexpr
      result
    end

    test "nested structure roundtrips" do
      sexpr =
        sx_atom(:foo, [
          sx_atom(:bar, [sx_atom0(:a), sx_atom0(:b)]),
          sx_atom(:baz, [sx_atom0(:c), sx_atom0(:d)])
        ])

      result = Sexpr.roundtrip_sexpr(sexpr)
      assert result == sexpr
      result
    end

    test "deeply nested structure roundtrips" do
      sexpr =
        sx_atom(:a, [
          sx_atom(:b, [
            sx_atom(:c, [
              sx_atom(:d, [
                sx_atom(:e, [
                  sx_atom0(:f)
                ])
              ])
            ])
          ])
        ])

      result = Sexpr.roundtrip_sexpr(sexpr)
      assert result == sexpr
      result
    end

    test "mixed structure with variables roundtrips" do
      sexpr =
        sx_atom(:root, [
          sx_var(:x),
          sx_atom(:branch, [sx_var(:y), sx_atom0(:leaf)]),
          sx_var(:z)
        ])

      result = Sexpr.roundtrip_sexpr(sexpr)
      assert result == sexpr
      result
    end
  end

  describe "roundtrip - term -> sexpr -> term" do
    test "variable term roundtrips" do
      term = tvv(:x)
      result = Sexpr.roundtrip_term(term)
      assert result == term
      result
    end

    test "nullary constructor roundtrips" do
      term = tvc0(:atom)
      result = Sexpr.roundtrip_term(term)
      assert result == term
      result
    end

    test "unary constructor roundtrips" do
      term = tvc(:f, [tvv(:x)])
      result = Sexpr.roundtrip_term(term)
      assert result == term
      result
    end

    test "binary constructor roundtrips" do
      term = tvc(:f, [tvv(:x), tvv(:y)])
      result = Sexpr.roundtrip_term(term)
      assert result == term
      result
    end

    test "ternary constructor roundtrips" do
      term = tvc(:f, [tvc0(:a), tvc0(:b), tvc0(:c)])
      result = Sexpr.roundtrip_term(term)
      assert result == term
      result
    end

    test "nested structure roundtrips" do
      term =
        tvc(:foo, [
          tvc(:bar, [tvc0(:a), tvc0(:b)]),
          tvc(:baz, [tvc0(:c), tvc0(:d)])
        ])

      result = Sexpr.roundtrip_term(term)
      assert result == term
      result
    end

    test "deeply nested structure roundtrips" do
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

      result = Sexpr.roundtrip_term(term)
      assert result == term
      result
    end

    test "mixed structure with variables roundtrips" do
      term =
        tvc(:root, [
          tvv(:x),
          tvc(:branch, [tvv(:y), tvc0(:leaf)]),
          tvv(:z)
        ])

      result = Sexpr.roundtrip_term(term)
      assert result == term
      result
    end
  end

  describe "depth" do
    test "depth of variable" do
      sexpr = sx_var(:x)
      assert Sexpr.depth(sexpr) == 0
    end

    test "depth of nullary atom" do
      sexpr = sx_atom0(:foo)
      assert Sexpr.depth(sexpr) == 1
    end

    test "depth of unary atom" do
      sexpr = sx_atom(:f, [sx_atom0(:x)])
      assert Sexpr.depth(sexpr) == 2
    end

    test "depth of binary atom" do
      sexpr = sx_atom(:f, [sx_atom0(:a), sx_atom0(:b)])
      assert Sexpr.depth(sexpr) == 2
    end

    test "depth of nested structure" do
      sexpr = sx_atom(:f, [sx_atom(:g, [sx_atom0(:h)])])
      assert Sexpr.depth(sexpr) == 3
    end

    test "depth of unbalanced structure" do
      sexpr =
        sx_atom(:root, [
          sx_atom0(:shallow),
          sx_atom(:branch, [
            sx_atom(:deep, [sx_atom0(:leaf)])
          ])
        ])

      assert Sexpr.depth(sexpr) == 4
    end
  end

  describe "size" do
    test "size of variable" do
      sexpr = sx_var(:x)
      assert Sexpr.size(sexpr) == 0
    end

    test "size of nullary atom" do
      sexpr = sx_atom0(:foo)
      assert Sexpr.size(sexpr) == 1
    end

    test "size of unary atom" do
      sexpr = sx_atom(:f, [sx_atom0(:x)])
      assert Sexpr.size(sexpr) == 2
    end

    test "size of binary atom" do
      sexpr = sx_atom(:f, [sx_atom0(:a), sx_atom0(:b)])
      assert Sexpr.size(sexpr) == 3
    end

    test "size of nested structure" do
      sexpr = sx_atom(:f, [sx_atom(:g, [sx_atom0(:a), sx_atom0(:b)])])
      assert Sexpr.size(sexpr) == 4
    end

    test "size with variables" do
      sexpr = sx_atom(:f, [sx_var(:x), sx_atom0(:a), sx_var(:y)])
      assert Sexpr.size(sexpr) == 2
    end
  end

  describe "map_atoms" do
    test "map over atoms in simple sexpr" do
      sexpr = sx_atom0(:foo)
      result = Sexpr.map_atoms(&String.to_atom("x_#{&1}"), sexpr)
      assert result == sx_atom0(:x_foo)
      result
    end

    test "map over atoms preserves variables" do
      sexpr = sx_atom(:f, [sx_var(:x), sx_atom0(:a)])

      result =
        Sexpr.map_atoms(
          &(String.upcase(Atom.to_string(&1)) |> String.to_atom()),
          sexpr
        )

      expected = sx_atom(:F, [sx_var(:x), sx_atom0(:A)])
      assert result == expected
      result
    end

    test "map over atoms in nested structure" do
      sexpr =
        sx_atom(:root, [
          sx_atom(:left, [sx_atom0(:a)]),
          sx_atom(:right, [sx_atom0(:b)])
        ])

      result = Sexpr.map_atoms(& &1, sexpr)
      assert result == sexpr
      result
    end
  end

  describe "map_vars" do
    test "map over variables in simple sexpr" do
      sexpr = sx_var(:x)
      result = Sexpr.map_vars(&String.to_atom("var_#{&1}"), sexpr)
      assert result == sx_var(:var_x)
      result
    end

    test "map over variables preserves atoms" do
      sexpr = sx_atom(:f, [sx_var(0), sx_atom0(:a)])
      result = Sexpr.map_vars(&(&1 + 1), sexpr)
      expected = sx_atom(:f, [sx_var(1), sx_atom0(:a)])
      assert result == expected
      result
    end

    test "map over variables in nested structure" do
      sexpr =
        sx_atom(:root, [
          sx_var(:x),
          sx_atom(:branch, [sx_var(:y)]),
          sx_var(:z)
        ])

      result =
        Sexpr.map_vars(
          &(String.upcase(Atom.to_string(&1)) |> String.to_atom()),
          sexpr
        )

      expected =
        sx_atom(:root, [
          sx_var(:X),
          sx_atom(:branch, [sx_var(:Y)]),
          sx_var(:Z)
        ])

      assert result == expected
      result
    end
  end

  describe "subst" do
    test "substitute single variable" do
      sexpr = sx_var(:x)
      result = Sexpr.subst(fn :x -> sx_atom0(:foo) end, sexpr)
      assert result == sx_atom0(:foo)
      result
    end

    test "substitute multiple variables" do
      sexpr = sx_atom(:plus, [sx_var(:x), sx_var(:y)])

      subst_fn = fn
        :x -> sx_atom0(:one)
        :y -> sx_atom0(:two)
      end

      result = Sexpr.subst(subst_fn, sexpr)
      expected = sx_atom(:plus, [sx_atom0(:one), sx_atom0(:two)])
      assert result == expected
      result
    end

    test "substitute with complex replacement" do
      sexpr = sx_atom(:f, [sx_var(:x)])

      subst_fn = fn :x ->
        sx_atom(:g, [sx_atom0(:a), sx_atom0(:b)])
      end

      result = Sexpr.subst(subst_fn, sexpr)
      expected = sx_atom(:f, [sx_atom(:g, [sx_atom0(:a), sx_atom0(:b)])])
      assert result == expected
      result
    end

    test "substitute in nested structure" do
      sexpr =
        sx_atom(:root, [
          sx_var(:x),
          sx_atom(:branch, [sx_var(:y)]),
          sx_var(:x)
        ])

      subst_fn = fn
        :x -> sx_atom0(:replaced_x)
        :y -> sx_atom0(:replaced_y)
      end

      result = Sexpr.subst(subst_fn, sexpr)

      expected =
        sx_atom(:root, [
          sx_atom0(:replaced_x),
          sx_atom(:branch, [sx_atom0(:replaced_y)]),
          sx_atom0(:replaced_x)
        ])

      assert result == expected
      result
    end
  end

  describe "close" do
    test "close open sexpr" do
      sexpr = sx_atom(:f, [sx_var(:x), sx_var(:y)])

      subst_fn = fn
        :x -> sx_atom0(:val_x)
        :y -> sx_atom0(:val_y)
      end

      result = Sexpr.close(subst_fn, sexpr)
      expected = sx_atom(:f, [sx_atom0(:val_x), sx_atom0(:val_y)])
      assert result == expected
      result
    end

    test "close nested open sexpr" do
      sexpr =
        sx_atom(:outer, [
          sx_var(:a),
          sx_atom(:inner, [sx_var(:b), sx_var(:c)])
        ])

      subst_fn = fn
        :a -> sx_atom0(1)
        :b -> sx_atom0(2)
        :c -> sx_atom0(3)
      end

      result = Sexpr.close(subst_fn, sexpr)

      expected =
        sx_atom(:outer, [
          sx_atom0(1),
          sx_atom(:inner, [sx_atom0(2), sx_atom0(3)])
        ])

      assert result == expected
      result
    end
  end

  describe "property: isomorphism" do
    test "multiple sexpr roundtrips reach fixpoint" do
      sexpr =
        sx_atom(:f, [
          sx_atom(:g, [sx_atom0(:a), sx_atom0(:b)]),
          sx_atom0(:c)
        ])

      r1 = Sexpr.roundtrip_sexpr(sexpr)
      r2 = Sexpr.roundtrip_sexpr(r1)
      r3 = Sexpr.roundtrip_sexpr(r2)

      assert r1 == sexpr
      assert r2 == sexpr
      assert r3 == sexpr
      r3
    end

    test "multiple term roundtrips reach fixpoint" do
      term =
        tvc(:f, [
          tvc(:g, [tvc0(:a), tvc0(:b)]),
          tvc0(:c)
        ])

      r1 = Sexpr.roundtrip_term(term)
      r2 = Sexpr.roundtrip_term(r1)
      r3 = Sexpr.roundtrip_term(r2)

      assert r1 == term
      assert r2 == term
      assert r3 == term
      r3
    end

    test "alternating roundtrips for sexpr" do
      sexpr = sx_atom(:root, [sx_atom0(:a), sx_atom0(:b), sx_atom0(:c)])

      term1 = Sexpr.to_term(sexpr)
      s1 = Sexpr.from_term(term1)
      term2 = Sexpr.to_term(s1)
      s2 = Sexpr.from_term(term2)

      assert term2 == term1
      assert s1 == sexpr
      assert s2 == sexpr
      s2
    end

    test "alternating roundtrips for term" do
      term = tvc(:root, [tvc0(:a), tvc0(:b), tvc0(:c)])

      s1 = Sexpr.from_term(term)
      term1 = Sexpr.to_term(s1)
      s2 = Sexpr.from_term(term1)
      term2 = Sexpr.to_term(s2)

      assert s2 == s1
      assert term1 == term
      assert term2 == term
      term2
    end
  end
end
