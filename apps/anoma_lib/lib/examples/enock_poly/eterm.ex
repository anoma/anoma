defmodule Examples.ENockPoly.ETerm do
  use Memoize

  import ExUnit.Assertions
  alias NockPoly.Term
  import NockPoly.Term.MacroDefs
  alias NockPoly.NockTerms

  use TypedStruct

  ####################################################################
  ##                        TERM TESTS                              ##
  ####################################################################
  @doc """
  t1: a closed term with no children.
  """
  def term_test_t1() do
    tvc0(1)
  end

  def term_test_t1_depth() do
    result = Term.depth(term_test_t1())
    assert result == 1
    result
  end

  def term_test_t1_size() do
    result = Term.size(term_test_t1())
    assert result == 1
    result
  end

  @doc """
  t2: a closed term with two children.
  """
  def term_test_t2() do
    tvc(:a, [tvc0(:a), tvc0(:b)])
  end

  def term_test_t2_depth() do
    result = Term.depth(term_test_t2())
    assert result == 2
    result
  end

  def term_test_t2_size() do
    result = Term.size(term_test_t2())
    assert result == 3
    result
  end

  @doc """
  t3: a nested closed term.
  """
  def term_test_t3() do
    tvc(:x, [tvc(:y, [tvc0(:z)])])
  end

  def term_test_t3_depth() do
    result = Term.depth(term_test_t3())
    assert result == 3
    result
  end

  def term_test_t3_size() do
    result = Term.size(term_test_t3())
    assert result == 3
    result
  end

  @doc """
  t4: a deeply nested closed term.
  """
  def term_test_t4() do
    tvc(
      0,
      [
        term_test_t1(),
        tvc(2, [tvc0(3), tvc0(4)]),
        tvc0(5)
      ]
    )
  end

  def term_test_t4_depth() do
    result = Term.depth(term_test_t4())
    assert result == 3
    result
  end

  def term_test_t4_size() do
    result = Term.size(term_test_t4())
    assert result == 6
    result
  end

  @doc """
  t8: a term tested with a custom cata algebra.
  An example of a direct call to `Term.cata/2`, since `depth` and
  `size` are defined in terms of `eval` and thus do not directly
  test the `cata` wrapper.  This also serves as an example of
  providing a custom algebra to `cata` (the algebra parameter to
  `eval` has the same signature, so it is also an example of that).
  In this example we use a string constructor type and concatenate
  all the constructor names.
  """
  def term_test_t8() do
    tvc("root", [tvc0("left"), tvc0("right")])
  end

  def term_test_t8_cata_string_algebra() do
    algebra = fn {ctor, children} ->
      to_string(ctor) <> Enum.join(children, "")
    end

    result = Term.cata(term_test_t8(), algebra)
    assert result == "rootleftright"
    result
  end

  defp slice_alg_nullary_only() do
    %{
      ctor: fn c -> {:ctor, c} end,
      empty: {:empty_list},
      cons: fn t, l -> {:cons_result, t, l} end,
      nonempty: fn nel -> nel end,
      term: fn ctor_r, list_r -> {:term, ctor_r, list_r} end
    }
  end

  def term_slice_eval_on_nullary() do
    term = tvc0(:foo)
    subst = fn v -> {:var, v} end

    result = Term.slice_eval(slice_alg_nullary_only(), subst, term)

    assert result == {:term, {:ctor, :foo}, {:empty_list}}
    result
  end

  def term_slice_eval_nullary_alg_on_term_with_children() do
    term = tvc(:root, [tvc0(:a), tvc0(:b)])
    subst = fn v -> {:var, v} end

    result = Term.slice_eval(slice_alg_nullary_only(), subst, term)

    assert result ==
             {:term, {:ctor, :root},
              {:cons_result, {:term, {:ctor, :a}, {:empty_list}},
               {:cons_result, {:term, {:ctor, :b}, {:empty_list}},
                {:empty_list}}}}

    result
  end

  defp slice_alg_variable_only() do
    %{
      ctor: fn c -> {:ctor_result, c} end,
      empty: {:empty_list},
      cons: fn t, l -> {:cons_result, t, l} end,
      nonempty: fn nel -> nel end,
      term: fn ctor_r, list_r -> {:term_result, ctor_r, list_r} end
    }
  end

  def term_slice_eval_on_variable() do
    term = tvv(42)
    subst = fn v -> {:var_result, v} end

    result = Term.slice_eval(slice_alg_variable_only(), subst, term)

    assert result == {:var_result, 42}
    result
  end

  def term_slice_eval_variable_alg_on_closed_term() do
    term = tvc(:foo, [tvc0(:a), tvc0(:b)])
    subst = fn v -> {:var_result, v} end

    result = Term.slice_eval(slice_alg_variable_only(), subst, term)

    assert result ==
             {:term_result, {:ctor_result, :foo},
              {:cons_result,
               {:term_result, {:ctor_result, :a}, {:empty_list}},
               {:cons_result,
                {:term_result, {:ctor_result, :b}, {:empty_list}},
                {:empty_list}}}}

    result
  end

  defp slice_alg_with_children() do
    %{
      ctor: fn c -> {:ctor_val, c} end,
      empty: [],
      cons: fn term_r, list_r -> [term_r | list_r] end,
      nonempty: fn nelist -> nelist end,
      term: fn ctor_r, list_r -> {ctor_r, list_r} end
    }
  end

  def term_slice_eval_on_binary_term() do
    term = tvc(:root, [tvc0(:left), tvc0(:right)])
    subst = fn v -> {:var, v} end

    result = Term.slice_eval(slice_alg_with_children(), subst, term)

    assert result ==
             {{:ctor_val, :root},
              [{{:ctor_val, :left}, []}, {{:ctor_val, :right}, []}]}

    result
  end

  def term_slice_eval_tracking_list_structure() do
    term = tvc(:f, [tvc0(:a), tvc0(:b), tvc0(:c)])

    slice_alg = %{
      ctor: fn c -> c end,
      empty: :empty,
      cons: fn t, l -> {:cons, t, l} end,
      nonempty: fn nel -> {:nonempty, nel} end,
      term: fn c, l -> {:term, c, l} end
    }

    subst = fn v -> {:var, v} end

    result = Term.slice_eval(slice_alg, subst, term)

    assert result ==
             {:term, :f,
              {:nonempty,
               {:cons, {:term, :a, :empty},
                {:nonempty,
                 {:cons, {:term, :b, :empty},
                  {:nonempty, {:cons, {:term, :c, :empty}, :empty}}}}}}}

    result
  end

  defp slice_alg_for_cata_tests() do
    %{
      ctor: fn c -> {:ctor_data, c} end,
      empty: nil,
      cons: fn t, l -> [t | l] end,
      nonempty: fn nel -> nel end,
      term: fn ctor_r, list_r -> {:result, ctor_r, list_r} end
    }
  end

  def term_slice_cata_on_nullary() do
    term = tvc0(:test)

    result = Term.slice_cata(term, slice_alg_for_cata_tests())
    assert result == {:result, {:ctor_data, :test}, nil}
    result
  end

  def term_slice_cata_alg_on_term_with_children() do
    term = tvc(:parent, [tvc0(:child1), tvc0(:child2)])

    result = Term.slice_cata(term, slice_alg_for_cata_tests())

    assert result ==
             {:result, {:ctor_data, :parent},
              [
                {:result, {:ctor_data, :child1}, nil},
                {:result, {:ctor_data, :child2}, nil} | nil
              ]}

    result
  end

  def term_slice_cata_on_binary_term() do
    term = tvc(:pair, [tvc0(:left), tvc0(:right)])

    slice_alg = %{
      ctor: fn c -> c end,
      empty: [],
      cons: fn t, l -> [t | l] end,
      nonempty: fn nel -> nel end,
      term: fn c, l -> {c, l} end
    }

    result = Term.slice_cata(term, slice_alg)
    assert result == {:pair, [{:left, []}, {:right, []}]}
    result
  end

  defp slice_alg_for_list_tests() do
    %{
      ctor: fn c -> c end,
      empty: :empty_marker,
      cons: fn t, l -> {:cons_marker, t, l} end,
      nonempty: fn nel -> nel end,
      term: fn c, l -> {c, l} end
    }
  end

  def term_slice_eval_list_empty() do
    terms = []
    subst = fn v -> {:var, v} end

    result = Term.slice_eval_list(slice_alg_for_list_tests(), subst, terms)
    assert result == :empty_marker
    result
  end

  def term_slice_eval_list_alg_on_nonempty_list() do
    terms = [tvc0(:a), tvc0(:b)]
    subst = fn v -> {:var, v} end

    result = Term.slice_eval_list(slice_alg_for_list_tests(), subst, terms)

    assert result ==
             {:cons_marker, {:a, :empty_marker},
              {:cons_marker, {:b, :empty_marker}, :empty_marker}}

    result
  end

  def term_slice_eval_list_single() do
    terms = [tvc0(:a)]

    slice_alg = %{
      ctor: fn c -> c end,
      empty: [],
      cons: fn t, l -> [t | l] end,
      nonempty: fn nel -> nel end,
      term: fn c, l -> {c, l} end
    }

    subst = fn v -> {:var, v} end

    result = Term.slice_eval_list(slice_alg, subst, terms)
    assert result == [{:a, []}]
    result
  end

  def term_slice_eval_list_multiple() do
    terms = [tvc0(:a), tvc0(:b), tvc0(:c)]

    slice_alg = %{
      ctor: fn c -> c end,
      empty: nil,
      cons: fn t, l -> {:cons, t, l} end,
      nonempty: fn nel -> nel end,
      term: fn c, l -> {c, l} end
    }

    subst = fn v -> {:var, v} end

    result = Term.slice_eval_list(slice_alg, subst, terms)

    assert result ==
             {:cons, {:a, nil}, {:cons, {:b, nil}, {:cons, {:c, nil}, nil}}}

    result
  end

  def term_slice_cata_list_multiple() do
    terms = [tvc0(:x), tvc0(:y)]

    slice_alg = %{
      ctor: fn c -> String.to_atom("processed_#{c}") end,
      empty: [],
      cons: fn t, l -> [t | l] end,
      nonempty: fn nel -> nel end,
      term: fn c, l -> {c, l} end
    }

    result = Term.slice_cata_list(terms, slice_alg)
    assert result == [{:processed_x, []}, {:processed_y, []}]
    result
  end

  @doc """
  v1: a single variable (an open term), which has depth and size 0.
  """
  def term_test_v1() do
    tvv("x")
  end

  def term_test_v1_depth() do
    result = Term.depth(term_test_v1())
    assert result == 0
    result
  end

  def term_test_v1_size() do
    result = Term.size(term_test_v1())
    assert result == 0
    result
  end

  @doc """
  I apply `out_tv` to a variable term to obtain the underlying representation.
  """
  def term_test_v1_out_tv() do
    out = Term.out_tv(term_test_v1())
    assert out == {:tvar, "x"}
    out
  end

  @doc """
  t5: an open term whose child is a variable.
  """
  def term_test_t5() do
    tvc(:a, [tvv("x")])
  end

  def term_test_t5_depth() do
    result = Term.depth(term_test_t5())
    assert result == 1
    result
  end

  def term_test_t5_size() do
    result = Term.size(term_test_t5())
    assert result == 1
    result
  end

  @doc """
  t6: an open term with multiple variables (one nested).
  """
  def term_test_t6() do
    tvc(
      :b,
      [
        tvv("x"),
        tvc(:c, [tvv("y"), tvv("z")])
      ]
    )
  end

  def term_test_t6_depth() do
    result = Term.depth(term_test_t6())
    assert result == 2
    result
  end

  def term_test_t6_size() do
    result = Term.size(term_test_t6())
    assert result == 2
    result
  end

  @doc """
  t7: an open term with no variables (a closed term viewed as open).
  """
  def term_test_t7() do
    tvc(:d, [tvc0(:e), tvc0(:f)])
  end

  def term_test_t7_depth() do
    result = Term.depth(term_test_t7())
    assert result == 2
    result
  end

  def term_test_t7_size() do
    result = Term.size(term_test_t7())
    assert result == 3
    result
  end

  @doc """
  I apply `out_tv` to a constructor term to obtain the underlying representation.
  """
  def term_test_t7_out_tv() do
    out = Term.out_tv(term_test_t7())
    assert out == tfc(:d, [tvc(:e, []), tvc(:f, [])])
    out
  end

  # Helper functions for termfv/tv tests
  defp add1(x), do: x + 1
  defp times2(x), do: x * 2

  # Helper function to transform terms
  defp term_times2(term), do: NockPoly.Term.tcmap(&times2/1, term)

  def termf_map_ctor_test() do
    term_f = {:foo, [1, 2, 3]}
    result = Term.termf_map_ctor(&String.to_atom("mapped_#{&1}"), term_f)
    assert result == {:mapped_foo, [1, 2, 3]}
    result
  end

  def termf_map_ctor_empty_children_test() do
    term_f = {:bar, []}
    result = Term.termf_map_ctor(&String.to_atom("mapped_#{&1}"), term_f)
    assert result == {:mapped_bar, []}
    result
  end

  def termf_bimap_test() do
    term_f = {:foo, [1, 2, 3]}

    result =
      Term.termf_bimap(
        &String.to_atom("mapped_#{&1}"),
        &(&1 * 10),
        term_f
      )

    assert result == {:mapped_foo, [10, 20, 30]}
    result
  end

  def termf_bimap_empty_children_test() do
    term_f = {:bar, []}

    result =
      Term.termf_bimap(
        &String.to_atom("mapped_#{&1}"),
        &Function.identity/1,
        term_f
      )

    assert result == {:mapped_bar, []}
    result
  end

  def termfv_map_ctor_on_variable_test() do
    term_fv = {:tvar, 42}
    result = Term.termfv_map_ctor(&Function.identity/1, term_fv)
    assert result == {:tvar, 42}
    result
  end

  def termfv_map_ctor_on_constructor_test() do
    term_fv = {:tcom, {:foo, [1, 2]}}
    result = Term.termfv_map_ctor(&String.to_atom("mapped_#{&1}"), term_fv)
    assert result == {:tcom, {:mapped_foo, [1, 2]}}
    result
  end

  def termfv_map_var_on_variable_test() do
    term_fv = {:tvar, 42}
    result = Term.termfv_map_var(&(&1 + 1), term_fv)
    assert result == {:tvar, 43}
    result
  end

  def termfv_map_var_on_constructor_test() do
    term_fv = {:tcom, {:foo, [1, 2]}}
    result = Term.termfv_map_var(&Function.identity/1, term_fv)
    assert result == {:tcom, {:foo, [1, 2]}}
    result
  end

  def termfv_map_on_variable_test() do
    term_fv = {:tvar, 42}
    result = Term.termfv_map(&Function.identity/1, term_fv)
    assert result == {:tvar, 42}
    result
  end

  def termfv_map_on_constructor_test() do
    term_fv = {:tcom, {:foo, [1, 2]}}
    result = Term.termfv_map(&(&1 * 10), term_fv)
    assert result == {:tcom, {:foo, [10, 20]}}
    result
  end

  def termfv_trimap_on_variable_test() do
    term_fv = {:tvar, 42}

    result =
      Term.termfv_trimap(
        &Function.identity/1,
        &(&1 + 1),
        &Function.identity/1,
        term_fv
      )

    assert result == {:tvar, 43}
    result
  end

  def termfv_trimap_on_constructor_test() do
    term_fv = {:tcom, {:foo, [1, 2]}}

    result =
      Term.termfv_trimap(
        &String.to_atom("mapped_#{&1}"),
        &Function.identity/1,
        &(&1 * 10),
        term_fv
      )

    assert result == {:tcom, {:mapped_foo, [10, 20]}}
    result
  end

  def termfv_bimap_variable_test() do
    res =
      NockPoly.Term.termfv_bimap(&add1/1, &times2/1, Term.out_tv(tvv(3)))

    assert res == Term.out_tv(tvv(4))
    Term.in_tv(res)
  end

  @doc """
  I test the application of `termfv_bimap` to a constructor term.
  """
  @spec termfv_bimap_constructor_test() ::
          NockPoly.Term.nat_tv(non_neg_integer())
  def termfv_bimap_constructor_test() do
    res =
      NockPoly.Term.termfv_bimap(
        &add1/1,
        &term_times2/1,
        Term.out_tv(tvc(3, [tvc0(1), tvc0(2)]))
      )

    expected = Term.out_tv(tvc(3, [tvc0(2), tvc0(4)]))
    assert res == expected
    Term.in_tv(res)
  end

  @doc """
  I test the use of `tvmap` to transform variables within a term.
  """
  def tvmap_test() do
    res =
      NockPoly.Term.tvmap(
        &add1/1,
        tvc(:a, [tvv(3), tvc(:b, [tvv(4)])])
      )

    assert res ==
             tvc(:a, [
               tvv(4),
               tvc(:b, [tvv(5)])
             ])

    res
  end

  @doc """
  I test `bimap` which maps both constructor and variable parameters.
  """
  def bimap_test() do
    term = tvc(:foo, [tvv(1), tvc(:bar, [tvv(2)])])

    result =
      NockPoly.Term.bimap(
        &String.to_atom("new_#{&1}"),
        &(&1 * 10),
        term
      )

    expected = tvc(:new_foo, [tvv(10), tvc(:new_bar, [tvv(20)])])
    assert result == expected
    result
  end

  @doc """
  I test `tv_comult` on the output of `termfv_bimap_variable_test`.

  The free monad law states that:
      tvmap(out_tv, tv_comult(term)) == term
  """
  def tv_comult_variable_from_bimap_test() do
    duplicated =
      NockPoly.Term.tv_comult(termfv_bimap_variable_test())

    assert duplicated == tvv(termfv_bimap_variable_test())
    duplicated
  end

  @doc """
  I test `tv_comult` on the output of `termfv_bimap_constructor_test`.

  The free monad law states that:
      tvmap(out_tv, tv_comult(term)) == term
  """
  def tv_comult_constructor_from_bimap_test() do
    term = termfv_bimap_constructor_test()
    duplicated = NockPoly.Term.tv_comult(term)
    result = NockPoly.Term.tvmap(&NockPoly.Term.out_tv/1, duplicated)
    assert result == term
    duplicated
  end

  @doc """
  I test `tv_mult` on the output of `tv_comult` for a variable term.

  The free monad law for join is that flattening a duplicated term (using `tv_mult`)
  returns the original term.
  """
  def tv_mult_variable_from_comult_test() do
    term = termfv_bimap_variable_test()
    duplicated = NockPoly.Term.tv_comult(term)
    flattened = NockPoly.Term.tv_mult(duplicated)
    assert flattened == term
    flattened
  end

  @doc """
  I test `tv_mult` on a larger term using the results of some previous examples.
  """
  def tv_mult_hybrid_test() do
    alias NockPoly.Term, as: T

    term =
      T.com_tv(:a, [
        termfv_bimap_variable_test(),
        T.com_tv(:b, [termfv_bimap_constructor_test(), term_test_t6()])
      ])

    duplicated = NockPoly.Term.tv_comult(term)
    flattened = NockPoly.Term.tv_mult(duplicated)
    assert flattened == term
    flattened
  end

  @doc """
  I test `tv_bind` on a variable term.
  """
  def tv_bind_variable_test() do
    f = fn x -> tvv(x + 1) end
    bound = NockPoly.Term.tv_bind(f, termfv_bimap_variable_test())
    assert bound == tvv(5)
    bound
  end

  @doc """
  I test `tv_bind` on a hybrid term.

  We construct a term using a variable term and a constructor term:
    - `var_term` is obtained from `termfv_bimap_variable_test()` (yielding a variable term with value 4).
    - `cons_term` is obtained from `termfv_bimap_constructor_test()` (yielding a constructor term of type :a with children [6, 8]).
  Then we define `m = T.com_tv(:c, [var_term, cons_term])`.
  We let `f` map any variable `x` to `T.com_tv(:b, [T.var_tv(x), T.var_tv(x + 10)])`.
  Thus:
    - For the variable branch (4), f returns a term with a constructor :b containing two variables with values 4 and 14.
    - For the constructor branch, the function is applied recursively to its children, transforming them accordingly.
  """
  def tv_bind_hybrid_test() do
    cons_term = termfv_bimap_constructor_test()

    m =
      tvc(:c, [termfv_bimap_variable_test(), cons_term])

    f = fn x ->
      tvc(:b, [tvv(x), tvv(x + 10)])
    end

    bound = NockPoly.Term.tv_bind(f, m)

    expected =
      tvc(
        :c,
        [
          tvc(:b, [tvv(4), tvv(14)]),
          cons_term
        ]
      )

    assert bound == expected
    bound
  end

  def substitute_test_variable() do
    closed_term =
      NockTerms.substitute(tvv(7), fn var ->
        tvc0({:atom, var + 1})
      end)

    noun = NockTerms.to_noun(closed_term)
    assert noun == 8
    closed_term
  end

  def substitute_test_cell() do
    closed_term =
      NockTerms.substitute(
        tvc(:cell, [tvv(7), tvc0({:atom, 99})]),
        fn v -> tvc0({:atom, v * 10}) end
      )

    {:ok, expected} = Noun.Format.parse("[70 99]")
    noun = NockTerms.to_noun(closed_term)
    assert noun == expected
    closed_term
  end

  @doc """
  I test `subst` which substitutes variables with open terms.
  """
  def subst_test() do
    term = tvc(:a, [tvv(1), tvv(2)])

    f = fn
      1 -> tvc(:b, [tvv(:x)])
      2 -> tvv(:y)
    end

    result = NockPoly.Term.subst(f, term)
    expected = tvc(:a, [tvc(:b, [tvv(:x)]), tvv(:y)])
    assert result == expected
    result
  end

  @doc """
  I test `full_subst` which substitutes all variables with closed terms.
  """
  def full_subst_test() do
    term = tvc(:a, [tvv(1), tvc(:b, [tvv(2)])])

    f = fn
      1 -> tvc0(:x)
      2 -> tvc0(:y)
    end

    result = NockPoly.Term.full_subst(f, term)
    expected = tvc(:a, [tvc0(:x), tvc(:b, [tvc0(:y)])])
    assert result == expected
    result
  end

  @doc """
  I test `eval_list` which evaluates a list of terms with an algebra.
  """
  def eval_list_test() do
    terms = [tvc(:a, [tvv(1)]), tvc(:b, [tvv(2), tvv(3)])]

    algebra = fn {ctor, children} ->
      {ctor, length(children)}
    end

    subst = fn v -> {:var, v} end

    result = NockPoly.Term.eval_list(algebra, subst, terms)
    expected = [{:a, 1}, {:b, 2}]
    assert result == expected
    result
  end

  @doc """
  I test `cata_list` which folds a list of closed terms.
  """
  def cata_list_test() do
    terms = [
      tvc(:node, [tvc0(:leaf), tvc0(:leaf)]),
      tvc0(:single)
    ]

    algebra = fn {_ctor, children} -> 1 + Enum.sum(children) end

    result = NockPoly.Term.cata_list(terms, algebra)
    expected = [3, 1]
    assert result == expected
    result
  end
end
