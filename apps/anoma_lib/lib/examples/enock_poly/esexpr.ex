defmodule Examples.ENockPoly.ESexpr do
  use Memoize

  import ExUnit.Assertions
  import NockPoly.Sexpr.MacroDefs
  import NockPoly.Term.MacroDefs

  alias NockPoly.Sexpr

  def basic_variable_sexpr() do
    sexpr = sx_var(:x)
    assert sexpr == {:var, :x}
    sexpr
  end

  def basic_nullary_atom_sexpr() do
    sexpr = sx_atom0(:foo)
    assert sexpr == {:atom, :foo, []}
    sexpr
  end

  def basic_unary_atom_sexpr() do
    sexpr = sx_atom(:succ, [sx_var(:n)])
    assert sexpr == {:atom, :succ, [{:var, :n}]}
    sexpr
  end

  def basic_binary_atom_sexpr() do
    sexpr = sx_atom(:plus, [sx_var(:x), sx_var(:y)])
    assert sexpr == {:atom, :plus, [{:var, :x}, {:var, :y}]}
    sexpr
  end

  def basic_nested_sexpr() do
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

  def to_term_variable() do
    sexpr = sx_var(:x)
    term = Sexpr.to_term(sexpr)
    assert term == tvv(:x)
    term
  end

  def to_term_nullary_atom() do
    sexpr = sx_atom0(:foo)
    term = Sexpr.to_term(sexpr)
    assert term == tvc0(:foo)
    term
  end

  def to_term_unary_atom() do
    sexpr = sx_atom(:f, [sx_var(:x)])
    term = Sexpr.to_term(sexpr)
    assert term == tvc(:f, [tvv(:x)])
    term
  end

  def to_term_binary_atom() do
    sexpr = sx_atom(:plus, [sx_var(:x), sx_var(:y)])
    term = Sexpr.to_term(sexpr)
    assert term == tvc(:plus, [tvv(:x), tvv(:y)])
    term
  end

  def to_term_nested_structure() do
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

  def to_term_deeply_nested_structure() do
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

  def from_term_variable() do
    term = tvv(:x)
    sexpr = Sexpr.from_term(term)
    assert sexpr == sx_var(:x)
    sexpr
  end

  def from_term_nullary_atom() do
    term = tvc0(:foo)
    sexpr = Sexpr.from_term(term)
    assert sexpr == sx_atom0(:foo)
    sexpr
  end

  def from_term_unary_atom() do
    term = tvc(:f, [tvv(:x)])
    sexpr = Sexpr.from_term(term)
    assert sexpr == sx_atom(:f, [sx_var(:x)])
    sexpr
  end

  def from_term_binary_atom() do
    term = tvc(:plus, [tvv(:x), tvv(:y)])
    sexpr = Sexpr.from_term(term)
    assert sexpr == sx_atom(:plus, [sx_var(:x), sx_var(:y)])
    sexpr
  end

  def from_term_nested_structure() do
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

  def from_term_deeply_nested_structure() do
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

  def roundtrip_sexpr_variable() do
    sexpr = sx_var(:x)
    result = Sexpr.roundtrip_sexpr(sexpr)
    assert result == sexpr
    result
  end

  def roundtrip_sexpr_nullary_atom() do
    sexpr = sx_atom0(:atom)
    result = Sexpr.roundtrip_sexpr(sexpr)
    assert result == sexpr
    result
  end

  def roundtrip_sexpr_unary_atom() do
    sexpr = sx_atom(:f, [sx_var(:x)])
    result = Sexpr.roundtrip_sexpr(sexpr)
    assert result == sexpr
    result
  end

  def roundtrip_sexpr_binary_atom() do
    sexpr = sx_atom(:f, [sx_var(:x), sx_var(:y)])
    result = Sexpr.roundtrip_sexpr(sexpr)
    assert result == sexpr
    result
  end

  def roundtrip_sexpr_ternary_atom() do
    sexpr = sx_atom(:f, [sx_atom0(:a), sx_atom0(:b), sx_atom0(:c)])
    result = Sexpr.roundtrip_sexpr(sexpr)
    assert result == sexpr
    result
  end

  def roundtrip_sexpr_nested_structure() do
    sexpr =
      sx_atom(:foo, [
        sx_atom(:bar, [sx_atom0(:a), sx_atom0(:b)]),
        sx_atom(:baz, [sx_atom0(:c), sx_atom0(:d)])
      ])

    result = Sexpr.roundtrip_sexpr(sexpr)
    assert result == sexpr
    result
  end

  def roundtrip_sexpr_deeply_nested_structure() do
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

  def roundtrip_sexpr_mixed_structure_with_variables() do
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

  def roundtrip_term_variable() do
    term = tvv(:x)
    result = Sexpr.roundtrip_term(term)
    assert result == term
    result
  end

  def roundtrip_term_nullary_constructor() do
    term = tvc0(:atom)
    result = Sexpr.roundtrip_term(term)
    assert result == term
    result
  end

  def roundtrip_term_unary_constructor() do
    term = tvc(:f, [tvv(:x)])
    result = Sexpr.roundtrip_term(term)
    assert result == term
    result
  end

  def roundtrip_term_binary_constructor() do
    term = tvc(:f, [tvv(:x), tvv(:y)])
    result = Sexpr.roundtrip_term(term)
    assert result == term
    result
  end

  def roundtrip_term_ternary_constructor() do
    term = tvc(:f, [tvc0(:a), tvc0(:b), tvc0(:c)])
    result = Sexpr.roundtrip_term(term)
    assert result == term
    result
  end

  def roundtrip_term_nested_structure() do
    term =
      tvc(:foo, [
        tvc(:bar, [tvc0(:a), tvc0(:b)]),
        tvc(:baz, [tvc0(:c), tvc0(:d)])
      ])

    result = Sexpr.roundtrip_term(term)
    assert result == term
    result
  end

  def roundtrip_term_deeply_nested_structure() do
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

  def roundtrip_term_mixed_structure_with_variables() do
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

  def depth_of_variable() do
    sexpr = sx_var(:x)
    result = Sexpr.depth(sexpr)
    assert result == 0
    result
  end

  def depth_of_nullary_atom() do
    sexpr = sx_atom0(:foo)
    result = Sexpr.depth(sexpr)
    assert result == 1
    result
  end

  def depth_of_unary_atom() do
    sexpr = sx_atom(:f, [sx_atom0(:x)])
    result = Sexpr.depth(sexpr)
    assert result == 2
    result
  end

  def depth_of_binary_atom() do
    sexpr = sx_atom(:f, [sx_atom0(:a), sx_atom0(:b)])
    result = Sexpr.depth(sexpr)
    assert result == 2
    result
  end

  def depth_of_nested_structure() do
    sexpr = sx_atom(:f, [sx_atom(:g, [sx_atom0(:h)])])
    result = Sexpr.depth(sexpr)
    assert result == 3
    result
  end

  def depth_of_unbalanced_structure() do
    sexpr =
      sx_atom(:root, [
        sx_atom0(:shallow),
        sx_atom(:branch, [
          sx_atom(:deep, [sx_atom0(:leaf)])
        ])
      ])

    result = Sexpr.depth(sexpr)
    assert result == 4
    result
  end

  def size_of_variable() do
    sexpr = sx_var(:x)
    result = Sexpr.size(sexpr)
    assert result == 0
    result
  end

  def size_of_nullary_atom() do
    sexpr = sx_atom0(:foo)
    result = Sexpr.size(sexpr)
    assert result == 1
    result
  end

  def size_of_unary_atom() do
    sexpr = sx_atom(:f, [sx_atom0(:x)])
    result = Sexpr.size(sexpr)
    assert result == 2
    result
  end

  def size_of_binary_atom() do
    sexpr = sx_atom(:f, [sx_atom0(:a), sx_atom0(:b)])
    result = Sexpr.size(sexpr)
    assert result == 3
    result
  end

  def size_of_nested_structure() do
    sexpr = sx_atom(:f, [sx_atom(:g, [sx_atom0(:a), sx_atom0(:b)])])
    result = Sexpr.size(sexpr)
    assert result == 4
    result
  end

  def size_with_variables() do
    sexpr = sx_atom(:f, [sx_var(:x), sx_atom0(:a), sx_var(:y)])
    result = Sexpr.size(sexpr)
    assert result == 2
    result
  end

  def map_atoms_simple_sexpr() do
    sexpr = sx_atom0(:foo)
    result = Sexpr.map_atoms(&String.to_atom("x_#{&1}"), sexpr)
    assert result == sx_atom0(:x_foo)
    result
  end

  def map_atoms_preserves_variables() do
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

  def map_atoms_nested_structure() do
    sexpr =
      sx_atom(:root, [
        sx_atom(:left, [sx_atom0(:a)]),
        sx_atom(:right, [sx_atom0(:b)])
      ])

    result = Sexpr.map_atoms(& &1, sexpr)
    assert result == sexpr
    result
  end

  def map_vars_simple_sexpr() do
    sexpr = sx_var(:x)
    result = Sexpr.map_vars(&String.to_atom("var_#{&1}"), sexpr)
    assert result == sx_var(:var_x)
    result
  end

  def map_vars_preserves_atoms() do
    sexpr = sx_atom(:f, [sx_var(0), sx_atom0(:a)])
    result = Sexpr.map_vars(&(&1 + 1), sexpr)
    expected = sx_atom(:f, [sx_var(1), sx_atom0(:a)])
    assert result == expected
    result
  end

  def map_vars_nested_structure() do
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

  def bimap_on_variable() do
    sexpr = sx_var(:x)

    result =
      Sexpr.bimap(
        &Function.identity/1,
        &String.to_atom("var_#{&1}"),
        sexpr
      )

    assert result == sx_var(:var_x)
    result
  end

  def bimap_on_atom_only() do
    sexpr = sx_atom(:foo, [sx_atom0(:a), sx_atom0(:b)])

    result =
      Sexpr.bimap(
        &String.to_atom("mapped_#{&1}"),
        &Function.identity/1,
        sexpr
      )

    expected =
      sx_atom(:mapped_foo, [sx_atom0(:mapped_a), sx_atom0(:mapped_b)])

    assert result == expected
    result
  end

  def bimap_on_mixed_structure() do
    sexpr =
      sx_atom(:root, [
        sx_var(0),
        sx_atom(:branch, [sx_var(1), sx_atom0(:leaf)]),
        sx_var(2)
      ])

    result =
      Sexpr.bimap(
        &String.to_atom("a_#{&1}"),
        &(&1 + 10),
        sexpr
      )

    expected =
      sx_atom(:a_root, [
        sx_var(10),
        sx_atom(:a_branch, [sx_var(11), sx_atom0(:a_leaf)]),
        sx_var(12)
      ])

    assert result == expected
    result
  end

  def subst_single_variable() do
    sexpr = sx_var(:x)
    result = Sexpr.subst(fn :x -> sx_atom0(:foo) end, sexpr)
    assert result == sx_atom0(:foo)
    result
  end

  def subst_multiple_variables() do
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

  def subst_with_nested_replacement() do
    sexpr = sx_atom(:f, [sx_var(:x)])

    subst_fn = fn :x ->
      sx_atom(:g, [sx_atom0(:a), sx_atom0(:b)])
    end

    result = Sexpr.subst(subst_fn, sexpr)
    expected = sx_atom(:f, [sx_atom(:g, [sx_atom0(:a), sx_atom0(:b)])])
    assert result == expected
    result
  end

  def subst_nested_structure() do
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

  def close_open_sexpr() do
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

  def close_nested_open_sexpr() do
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

  def closed_sexpr_nullary() do
    closed = sx_closed0(:foo)
    assert closed == :foo
    closed
  end

  def closed_sexpr_unary() do
    closed = sx_closed(:f, [sx_closed0(:x)])
    assert closed == {:f, [:x]}
    closed
  end

  def closed_sexpr_binary() do
    closed = sx_closed(:f, [sx_closed0(:a), sx_closed0(:b)])
    assert closed == {:f, [:a, :b]}
    closed
  end

  def closed_sexpr_nested() do
    closed =
      sx_closed(:root, [
        sx_closed(:left, [sx_closed0(:a)]),
        sx_closed(:right, [sx_closed0(:b)])
      ])

    assert closed == {:root, [{:left, [:a]}, {:right, [:b]}]}
    closed
  end

  def closed_to_open_nullary() do
    closed = sx_closed0(:foo)
    open = Sexpr.closed_to_open(closed)
    assert open == sx_atom0(:foo)
    open
  end

  def closed_to_open_unary() do
    closed = sx_closed(:f, [sx_closed0(:x)])
    open = Sexpr.closed_to_open(closed)
    assert open == sx_atom(:f, [sx_atom0(:x)])
    open
  end

  def closed_to_open_nested() do
    closed =
      sx_closed(:root, [
        sx_closed(:branch, [sx_closed0(:leaf)])
      ])

    open = Sexpr.closed_to_open(closed)

    expected =
      sx_atom(:root, [
        sx_atom(:branch, [sx_atom0(:leaf)])
      ])

    assert open == expected
    open
  end

  def closed_to_open_list_empty() do
    result = Sexpr.closed_to_open_list([])
    assert result == []
    result
  end

  def closed_to_open_list_nonempty() do
    closed_list = [
      sx_closed0(:a),
      sx_closed(:b, [sx_closed0(:c)])
    ]

    result = Sexpr.closed_to_open_list(closed_list)

    expected = [
      sx_atom0(:a),
      sx_atom(:b, [sx_atom0(:c)])
    ]

    assert result == expected
    result
  end

  def open_to_closed_nullary() do
    open = sx_atom0(:foo)
    closed = Sexpr.open_to_closed(open)
    assert closed == sx_closed0(:foo)
    closed
  end

  def open_to_closed_unary() do
    open = sx_atom(:f, [sx_atom0(:x)])
    closed = Sexpr.open_to_closed(open)
    assert closed == sx_closed(:f, [sx_closed0(:x)])
    closed
  end

  def open_to_closed_nested() do
    open =
      sx_atom(:root, [
        sx_atom(:branch, [sx_atom0(:leaf)])
      ])

    closed = Sexpr.open_to_closed(open)

    expected =
      sx_closed(:root, [
        sx_closed(:branch, [sx_closed0(:leaf)])
      ])

    assert closed == expected
    closed
  end

  def roundtrip_closed_nullary() do
    closed = sx_closed0(:atom)
    result = Sexpr.roundtrip_closed(closed)
    assert result == closed
    result
  end

  def roundtrip_closed_unary() do
    closed = sx_closed(:f, [sx_closed0(:x)])
    result = Sexpr.roundtrip_closed(closed)
    assert result == closed
    result
  end

  def roundtrip_closed_binary() do
    closed = sx_closed(:f, [sx_closed0(:a), sx_closed0(:b)])
    result = Sexpr.roundtrip_closed(closed)
    assert result == closed
    result
  end

  def roundtrip_closed_ternary() do
    closed = sx_closed(:f, [sx_closed0(:a), sx_closed0(:b), sx_closed0(:c)])
    result = Sexpr.roundtrip_closed(closed)
    assert result == closed
    result
  end

  def roundtrip_closed_nested_structure() do
    closed =
      sx_closed(:root, [
        sx_closed(:left, [sx_closed0(:a), sx_closed0(:b)]),
        sx_closed(:right, [sx_closed0(:c)])
      ])

    result = Sexpr.roundtrip_closed(closed)
    assert result == closed
    result
  end

  def roundtrip_closed_deeply_nested_structure() do
    closed =
      sx_closed(:a, [
        sx_closed(:b, [
          sx_closed(:c, [
            sx_closed(:d, [
              sx_closed0(:e)
            ])
          ])
        ])
      ])

    result = Sexpr.roundtrip_closed(closed)
    assert result == closed
    result
  end

  def roundtrip_open_closed_nullary() do
    open = sx_atom0(:atom)
    result = Sexpr.roundtrip_open_closed(open)
    assert result == open
    result
  end

  def roundtrip_open_closed_unary() do
    open = sx_atom(:f, [sx_atom0(:x)])
    result = Sexpr.roundtrip_open_closed(open)
    assert result == open
    result
  end

  def roundtrip_open_closed_binary() do
    open = sx_atom(:f, [sx_atom0(:a), sx_atom0(:b)])
    result = Sexpr.roundtrip_open_closed(open)
    assert result == open
    result
  end

  def roundtrip_open_closed_nested_structure() do
    open =
      sx_atom(:root, [
        sx_atom(:left, [sx_atom0(:a), sx_atom0(:b)]),
        sx_atom(:right, [sx_atom0(:c)])
      ])

    result = Sexpr.roundtrip_open_closed(open)
    assert result == open
    result
  end

  def alternating_roundtrips_closed_open_closed() do
    closed = sx_closed(:root, [sx_closed0(:a), sx_closed0(:b)])

    open1 = Sexpr.closed_to_open(closed)
    c1 = Sexpr.open_to_closed(open1)
    open2 = Sexpr.closed_to_open(c1)
    c2 = Sexpr.open_to_closed(open2)

    assert open2 == open1
    assert c1 == closed
    assert c2 == closed
    c2
  end

  def alternating_roundtrips_open_closed_open() do
    open = sx_atom(:root, [sx_atom0(:a), sx_atom0(:b)])

    c1 = Sexpr.open_to_closed(open)
    open1 = Sexpr.closed_to_open(c1)
    c2 = Sexpr.open_to_closed(open1)
    open2 = Sexpr.closed_to_open(c2)

    assert c2 == c1
    assert open1 == open
    assert open2 == open
    open2
  end

  def multiple_closed_roundtrips_reach_fixpoint() do
    closed = sx_closed(:f, [sx_closed(:g, [sx_closed0(:h)])])

    r1 = Sexpr.roundtrip_closed(closed)
    r2 = Sexpr.roundtrip_closed(r1)
    r3 = Sexpr.roundtrip_closed(r2)

    assert r1 == closed
    assert r2 == closed
    assert r3 == closed
    r3
  end

  def multiple_open_closed_roundtrips_reach_fixpoint() do
    open = sx_atom(:f, [sx_atom(:g, [sx_atom0(:h)])])

    r1 = Sexpr.roundtrip_open_closed(open)
    r2 = Sexpr.roundtrip_open_closed(r1)
    r3 = Sexpr.roundtrip_open_closed(r2)

    assert r1 == open
    assert r2 == open
    assert r3 == open
    r3
  end

  def multiple_sexpr_roundtrips_reach_fixpoint() do
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

  def multiple_term_roundtrips_reach_fixpoint() do
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

  def alternating_roundtrips_for_sexpr() do
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

  def alternating_roundtrips_for_term() do
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

  defp slice_alg_nullary_only() do
    %{
      atom: fn a -> {:atom_result, a} end,
      empty: {:empty_list},
      cons: fn s, l -> {:cons_result, s, l} end,
      nonempty: fn nel -> nel end,
      sexpr: fn atom_r, list_r -> {:sexpr_result, atom_r, list_r} end
    }
  end

  def sexpr_slice_eval_on_nullary() do
    sexpr = sx_atom0(:foo)
    subst = fn v -> {:var_result, v} end

    result = Sexpr.slice_eval(slice_alg_nullary_only(), subst, sexpr)

    assert result ==
             {:sexpr_result, {:atom_result, :foo}, {:empty_list}}

    result
  end

  def sexpr_slice_eval_nullary_alg_on_sexpr_with_children() do
    sexpr = sx_atom(:root, [sx_atom0(:a), sx_atom0(:b)])
    subst = fn v -> {:var_result, v} end

    result = Sexpr.slice_eval(slice_alg_nullary_only(), subst, sexpr)

    assert result ==
             {:sexpr_result, {:atom_result, :root},
              {:cons_result,
               {:sexpr_result, {:atom_result, :a}, {:empty_list}},
               {:cons_result,
                {:sexpr_result, {:atom_result, :b}, {:empty_list}},
                {:empty_list}}}}

    result
  end

  defp slice_alg_variable_only() do
    %{
      atom: fn a -> {:atom_result, a} end,
      empty: {:empty_list},
      cons: fn s, l -> {:cons_result, s, l} end,
      nonempty: fn nel -> nel end,
      sexpr: fn atom_r, list_r -> {:sexpr_result, atom_r, list_r} end
    }
  end

  def sexpr_slice_eval_on_variable() do
    sexpr = sx_var(42)
    subst = fn v -> {:var_result, v} end

    result = Sexpr.slice_eval(slice_alg_variable_only(), subst, sexpr)

    assert result == {:var_result, 42}
    result
  end

  def sexpr_slice_eval_variable_alg_on_closed_sexpr() do
    sexpr = sx_atom(:foo, [sx_atom0(:a), sx_atom0(:b)])
    subst = fn v -> {:var_result, v} end

    result = Sexpr.slice_eval(slice_alg_variable_only(), subst, sexpr)

    assert result ==
             {:sexpr_result, {:atom_result, :foo},
              {:cons_result,
               {:sexpr_result, {:atom_result, :a}, {:empty_list}},
               {:cons_result,
                {:sexpr_result, {:atom_result, :b}, {:empty_list}},
                {:empty_list}}}}

    result
  end

  defp slice_alg_with_children() do
    %{
      atom: fn a -> {:atom_val, a} end,
      empty: [],
      cons: fn s, l -> [s | l] end,
      nonempty: fn nel -> nel end,
      sexpr: fn atom_r, list_r -> {atom_r, list_r} end
    }
  end

  def sexpr_slice_eval_on_binary_sexpr() do
    sexpr = sx_atom(:root, [sx_atom0(:left), sx_atom0(:right)])
    subst = fn v -> {:var, v} end

    result = Sexpr.slice_eval(slice_alg_with_children(), subst, sexpr)

    assert result ==
             {{:atom_val, :root},
              [{{:atom_val, :left}, []}, {{:atom_val, :right}, []}]}

    result
  end

  def sexpr_slice_eval_tracking_list_structure() do
    sexpr = sx_atom(:f, [sx_atom0(:a), sx_atom0(:b), sx_atom0(:c)])

    slice_alg = %{
      atom: fn a -> a end,
      empty: :empty,
      cons: fn s, l -> {:cons, s, l} end,
      nonempty: fn nel -> {:nonempty, nel} end,
      sexpr: fn a, l -> {:sexpr, a, l} end
    }

    subst = fn v -> {:var, v} end

    result = Sexpr.slice_eval(slice_alg, subst, sexpr)

    assert result ==
             {:sexpr, :f,
              {:nonempty,
               {:cons, {:sexpr, :a, :empty},
                {:nonempty,
                 {:cons, {:sexpr, :b, :empty},
                  {:nonempty, {:cons, {:sexpr, :c, :empty}, :empty}}}}}}}

    result
  end

  defp slice_alg_for_cata_tests() do
    %{
      atom: fn a -> {:atom_data, a} end,
      empty: nil,
      cons: fn s, l -> [s | l] end,
      nonempty: fn nel -> nel end,
      sexpr: fn atom_r, list_r -> {:result, atom_r, list_r} end
    }
  end

  def sexpr_slice_cata_on_nullary() do
    sexpr = sx_atom0(:test)

    result = Sexpr.slice_cata(sexpr, slice_alg_for_cata_tests())
    assert result == {:result, {:atom_data, :test}, nil}
    result
  end

  def sexpr_slice_cata_alg_on_sexpr_with_children() do
    sexpr = sx_atom(:parent, [sx_atom0(:child1), sx_atom0(:child2)])

    result = Sexpr.slice_cata(sexpr, slice_alg_for_cata_tests())

    assert result ==
             {:result, {:atom_data, :parent},
              [
                {:result, {:atom_data, :child1}, nil},
                {:result, {:atom_data, :child2}, nil} | nil
              ]}

    result
  end

  def sexpr_slice_cata_on_binary_sexpr() do
    sexpr = sx_atom(:pair, [sx_atom0(:left), sx_atom0(:right)])

    slice_alg = %{
      atom: fn a -> a end,
      empty: [],
      cons: fn s, l -> [s | l] end,
      nonempty: fn nel -> nel end,
      sexpr: fn a, l -> {a, l} end
    }

    result = Sexpr.slice_cata(sexpr, slice_alg)
    assert result == {:pair, [{:left, []}, {:right, []}]}
    result
  end

  defp slice_alg_for_list_tests() do
    %{
      atom: fn a -> a end,
      empty: :empty_marker,
      cons: fn s, l -> {:cons_marker, s, l} end,
      nonempty: fn nel -> nel end,
      sexpr: fn a, l -> {a, l} end
    }
  end

  def sexpr_slice_eval_list_empty() do
    sexprs = []
    subst = fn v -> {:var, v} end

    result = Sexpr.slice_eval_list(slice_alg_for_list_tests(), subst, sexprs)
    assert result == :empty_marker
    result
  end

  def sexpr_slice_eval_list_alg_on_nonempty_list() do
    sexprs = [sx_atom0(:a), sx_atom0(:b)]
    subst = fn v -> {:var, v} end

    result = Sexpr.slice_eval_list(slice_alg_for_list_tests(), subst, sexprs)

    assert result ==
             {:cons_marker, {:a, :empty_marker},
              {:cons_marker, {:b, :empty_marker}, :empty_marker}}

    result
  end

  def sexpr_slice_eval_list_single() do
    sexprs = [sx_atom0(:a)]

    slice_alg = %{
      atom: fn a -> a end,
      empty: [],
      cons: fn s, l -> [s | l] end,
      nonempty: fn nel -> nel end,
      sexpr: fn a, l -> {a, l} end
    }

    subst = fn v -> {:var, v} end

    result = Sexpr.slice_eval_list(slice_alg, subst, sexprs)
    assert result == [{:a, []}]
    result
  end

  def sexpr_slice_eval_list_multiple() do
    sexprs = [sx_atom0(:a), sx_atom0(:b), sx_atom0(:c)]

    slice_alg = %{
      atom: fn a -> a end,
      empty: nil,
      cons: fn s, l -> {:cons, s, l} end,
      nonempty: fn nel -> nel end,
      sexpr: fn a, l -> {a, l} end
    }

    subst = fn v -> {:var, v} end

    result = Sexpr.slice_eval_list(slice_alg, subst, sexprs)

    assert result ==
             {:cons, {:a, nil}, {:cons, {:b, nil}, {:cons, {:c, nil}, nil}}}

    result
  end

  def sexpr_slice_cata_list_multiple() do
    sexprs = [sx_atom0(:x), sx_atom0(:y)]

    slice_alg = %{
      atom: fn a -> String.to_atom("processed_#{a}") end,
      empty: [],
      cons: fn s, l -> [s | l] end,
      nonempty: fn nel -> nel end,
      sexpr: fn a, l -> {a, l} end
    }

    result = Sexpr.slice_cata_list(sexprs, slice_alg)
    assert result == [{:processed_x, []}, {:processed_y, []}]
    result
  end

  def sexpr_eval_list_empty() do
    sexprs = []
    alg = fn {a, _children} -> a end
    subst = fn _v -> :replaced end

    result = Sexpr.eval_list(alg, subst, sexprs)
    assert result == []
    result
  end

  def sexpr_eval_list_nonempty() do
    sexprs = [sx_atom0(:x), sx_var(:v), sx_atom(:y, [sx_var(:w)])]
    alg = fn {a, children} -> {a, children} end
    subst = fn v -> {:replaced, v} end

    result = Sexpr.eval_list(alg, subst, sexprs)

    assert result == [
             {:x, []},
             {:replaced, :v},
             {:y, [{:replaced, :w}]}
           ]

    result
  end

  def sexpr_cata_list_empty() do
    sexprs = []
    alg = fn {a, _children} -> a end

    result = Sexpr.cata_list(sexprs, alg)
    assert result == []
    result
  end

  def sexpr_cata_list_nonempty() do
    sexprs = [sx_atom0(:a), sx_atom(:b, [sx_atom0(:c)])]
    alg = fn {a, children} -> {a, length(children)} end

    result = Sexpr.cata_list(sexprs, alg)
    assert result == [{:a, 0}, {:b, 1}]
    result
  end

  def sexpr_to_term_uses_slice_algebra() do
    sexpr = sx_atom(:test, [sx_atom0(:a), sx_atom0(:b)])

    result = Sexpr.to_term(sexpr)
    expected = tvc(:test, [tvc0(:a), tvc0(:b)])

    assert result == expected
    result
  end
end
