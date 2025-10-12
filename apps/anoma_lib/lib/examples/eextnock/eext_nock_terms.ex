defmodule Examples.EExtNock.EExtNockTerms do
  @moduledoc """
  I include examples and tests for Nock macros.

  Many of the tests in this module are based on test cases from example
  Nock interpreters available on the Urbit website, particularly the JavaScript
  reference implementation.
  """

  import ExtNock
  alias ExtNock.ExtNockTerms
  use TypedStruct
  import ExUnit.Assertions

  alias NockPoly.Term
  import Term.MacroDefs

  @doc """
  I test conversion from the sexpr representation to open_nock_poly_term
  for a lone atom.
  """
  def nock_poly_sexpr_atom_test() do
    term = ExtNockTerms.from_sexpr!(42)
    assert term == tvc0({:atom, 42})
    term
  end

  @doc """
  I test conversion from the sexpr representation to open_nock_poly_term for a
  lone variable.
  """
  def nock_poly_sexpr_variable_test() do
    term = ExtNockTerms.from_sexpr!({:var, 7})
    assert term == tvv(7)
    term
  end

  @doc """
  I test conversion from the sexpr representation to open_nock_poly_term
  for a cell.
  """
  def nock_poly_sexpr_cell_test() do
    ExtNockTerms.from_sexpr!([1, 2])
  end

  def nock_poly_sexpr_cell_test_structure() do
    expected =
      tvc(:cell, [
        tvc0({:atom, 1}),
        tvc0({:atom, 2})
      ])

    result = nock_poly_sexpr_cell_test()
    assert result == expected
    result
  end

  def nock_poly_sexpr_cell_test_to_noun() do
    expected_noun = Noun.Format.parse_always("[1 2]")
    result = ExtNockTerms.to_noun!(nock_poly_sexpr_cell_test())
    assert result == expected_noun
    result
  end

  @doc """
  I test conversion from the sexpr representation to open_nock_poly_term for a
  nested expression.
  """
  def nock_poly_sexpr_nested_test() do
    ExtNockTerms.from_sexpr!([[4, 5], [12, 13], 7])
  end

  def nock_poly_sexpr_nested_test_structure() do
    expected_inner1 =
      tvc(:cell, [
        tvc0({:atom, 4}),
        tvc0({:atom, 5})
      ])

    expected_inner2 =
      tvc(:cell, [
        tvc0({:atom, 12}),
        tvc0({:atom, 13})
      ])

    expected =
      tvc(:cell, [
        expected_inner1,
        tvc(:cell, [
          expected_inner2,
          tvc0({:atom, 7})
        ])
      ])

    result = nock_poly_sexpr_nested_test()
    assert result == expected
    result
  end

  def nock_poly_sexpr_nested_test_to_noun() do
    expected_noun = Noun.Format.parse_always("[[4 5] [12 13] 7]")
    result = ExtNockTerms.to_noun!(nock_poly_sexpr_nested_test())
    assert result == expected_noun
    result
  end

  @doc """
  I test conversion from the sexpr representation to open_nock_poly_term for a
  term with variables.
  """
  def nock_poly_sexpr_with_variables_test() do
    ExtNockTerms.from_sexpr!([{:var, 7}, 99])
  end

  def nock_poly_sexpr_with_variables_test_structure() do
    expected = tvc(:cell, [tvv(7), tvc0({:atom, 99})])
    result = nock_poly_sexpr_with_variables_test()
    assert result == expected
    result
  end

  def nock_poly_sexpr_with_variables_test_substitute() do
    closed_term =
      ExtNockTerms.substitute(
        nock_poly_sexpr_with_variables_test(),
        fn v -> tvc0({:atom, v * 10}) end
      )

    expected_noun = Noun.Format.parse_always("[70 99]")
    result = ExtNockTerms.to_noun!(closed_term)
    assert result == expected_noun
    result
  end

  @doc """
  I confirm that an empty s-expression should not represent any Nock
  polynomial term.
  """
  def nock_poly_sexpr_empty_list_test() do
    assert ExtNockTerms.from_sexpr([]) == :error
  end

  @doc """
  I illustrate an s-expression which can not be interpreted as any Nock
  polynomial term.
  """
  def nock_poly_sexpr_behavior_test() do
    invalid_sexpr = [[{:unknown, "x"}], 2]
    assert ExtNockTerms.from_sexpr(invalid_sexpr) == :error
  end

  @doc """
  I show that `from_sexpr!` raises an exception when given invalid input.
  """
  def nock_poly_sexpr_bang_error_test() do
    invalid_sexpr = [[{:unknown, "x"}], 2]

    assert_raise RuntimeError, fn ->
      ExtNockTerms.from_sexpr!(invalid_sexpr)
    end
  end

  @doc """
  I test the :slot constructor which compiles to Nock formula 0 (slot).
  """
  def slot_test_subject() do
    Noun.Format.parse_always("[[2 0] [3 1]]")
  end

  def slot_1_formula() do
    ExtNockTerms.sexpr_to_noun!({:slot, [1]})
  end

  def slot_1_result() do
    {:ok, result} = Nock.nock(slot_test_subject(), slot_1_formula())
    assert result == slot_test_subject()
    result
  end

  def slot_2_formula() do
    ExtNockTerms.sexpr_to_noun!({:slot, [2]})
  end

  def slot_2_result() do
    {:ok, result} = Nock.nock(slot_test_subject(), slot_2_formula())
    assert result == [2 | 0]
    result
  end

  def slot_3_formula() do
    ExtNockTerms.sexpr_to_noun!({:slot, [3]})
  end

  def slot_3_result() do
    {:ok, result} = Nock.nock(slot_test_subject(), slot_3_formula())
    assert result == [3 | 1]
    result
  end

  def slot_4_formula() do
    ExtNockTerms.sexpr_to_noun!({:slot, [4]})
  end

  def slot_4_result() do
    {:ok, result} = Nock.nock(slot_test_subject(), slot_4_formula())
    assert result == 2
    result
  end

  def slot_5_formula() do
    ExtNockTerms.sexpr_to_noun!({:slot, [5]})
  end

  def slot_5_result() do
    {:ok, result} = Nock.nock(slot_test_subject(), slot_5_formula())
    assert result == 0
    result
  end

  def slot_6_formula() do
    ExtNockTerms.sexpr_to_noun!({:slot, [6]})
  end

  def slot_6_result() do
    {:ok, result} = Nock.nock(slot_test_subject(), slot_6_formula())
    assert result == 3
    result
  end

  def slot_7_formula() do
    ExtNockTerms.sexpr_to_noun!({:slot, [7]})
  end

  def slot_7_result() do
    {:ok, result} = Nock.nock(slot_test_subject(), slot_7_formula())
    assert result == 1
    result
  end

  def slot_8_formula() do
    ExtNockTerms.sexpr_to_noun!({:slot, [8]})
  end

  def slot_8_result_error() do
    result = Nock.nock(slot_test_subject(), slot_8_formula())
    assert {:error, _} = result
    result
  end

  @doc """
  I test the :constant constructor which compiles to Nock formula 1 (constant).
  """
  def constant_atom_formula() do
    ExtNockTerms.sexpr_to_noun!({:constant, [42]})
  end

  def constant_atom_result_subject_0() do
    {:ok, result} = Nock.nock(0, constant_atom_formula())
    assert result == 42
    result
  end

  def constant_atom_result_subject_cell() do
    {:ok, result} = Nock.nock([1, 2], constant_atom_formula())
    assert result == 42
    result
  end

  def constant_cell_formula() do
    ExtNockTerms.sexpr_to_noun!({:constant, [[1, 2]]})
  end

  def constant_cell_result() do
    {:ok, result} = Nock.nock(0, constant_cell_formula())
    assert result == [1 | 2]
    result
  end

  @doc """
  I test error cases for the :slot constructor.

  I verify that errors are properly detected for various invalid inputs
  and operations on malformed slot terms.
  """
  def slot_constructor_error_test() do
    # Empty argument list
    assert ExtNockTerms.from_sexpr({:slot, []}) == :error

    # Too many arguments
    assert ExtNockTerms.from_sexpr({:slot, [1, 2]}) == :error

    # Invalid address
    assert ExtNockTerms.from_sexpr({:slot, [{:invalid_term}]}) == :error

    # Invalid nested argument
    assert ExtNockTerms.from_sexpr({:slot, [{:unknown, "x"}]}) == :error

    # from_sexpr! with invalid input
    assert_raise RuntimeError, fn ->
      ExtNockTerms.from_sexpr!({:slot, []})
    end

    # Malformed term structure for typechecking
    malformed_term = tvc0(:slot)

    assert {:error, _errors} =
             ExtNockTerms.compile_to_nock_term(malformed_term)

    # compile_to_nock_term! with invalid term
    assert_raise RuntimeError, fn ->
      ExtNockTerms.compile_to_nock_term!(malformed_term)
    end

    # to_noun with invalid term
    assert {:error, _errors} = ExtNockTerms.to_noun(malformed_term)

    # to_noun! with invalid term
    assert_raise RuntimeError, fn ->
      ExtNockTerms.to_noun!(malformed_term)
    end
  end

  @doc """
  I test error cases for the :constant constructor.

  I verify that errors are properly detected for various invalid inputs
  and operations on malformed constant terms.
  """
  def constant_constructor_error_test() do
    # Empty argument list
    assert ExtNockTerms.from_sexpr({:constant, []}) == :error

    # Too many arguments
    assert ExtNockTerms.from_sexpr({:constant, [1, 2]}) == :error

    # Invalid nested argument
    assert ExtNockTerms.from_sexpr({:constant, [{:unknown, "x"}]}) == :error

    # from_sexpr! with invalid input
    assert_raise RuntimeError, fn ->
      ExtNockTerms.from_sexpr!({:constant, []})
    end

    # Malformed term structure for typechecking
    malformed_term = tvc0(:constant)

    assert {:error, _errors} =
             ExtNockTerms.compile_to_nock_term(malformed_term)

    # compile_to_nock_term! with invalid term
    assert_raise RuntimeError, fn ->
      ExtNockTerms.compile_to_nock_term!(malformed_term)
    end

    # to_noun with invalid term
    assert {:error, _errors} = ExtNockTerms.to_noun(malformed_term)

    # to_noun! with invalid term
    assert_raise RuntimeError, fn ->
      ExtNockTerms.to_noun!(malformed_term)
    end

    # sexpr_to_noun with invalid s-expression
    assert ExtNockTerms.sexpr_to_noun({:constant, []}) == :error

    # sexpr_to_noun! with invalid s-expression
    assert_raise RuntimeError, fn ->
      ExtNockTerms.sexpr_to_noun!({:constant, []})
    end
  end

  @doc """
  I test the :evaluate constructor (Nock formula 2).
  """
  def evaluate_formula_1() do
    ExtNockTerms.sexpr_to_noun!(
      {:evaluate,
       [
         {:constant, [2]},
         {:constant, [{:slot, [1]}]}
       ]}
    )
  end

  def evaluate_formula_1_result() do
    {:ok, result} = Nock.nock(1, evaluate_formula_1())
    assert result == 2
    result
  end

  def evaluate_formula_2() do
    ExtNockTerms.sexpr_to_noun!(
      {:evaluate,
       [
         {:constant, [42]},
         {:constant, [{:constant, [153]}]}
       ]}
    )
  end

  def evaluate_formula_2_result() do
    {:ok, result} = Nock.nock(77, evaluate_formula_2())
    assert result == 153
    result
  end

  @doc """
  I test the :cell_test constructor (Nock formula 3).
  """
  def cell_test_atom_formula() do
    ExtNockTerms.sexpr_to_noun!({:cell_test, [{:constant, [1]}]})
  end

  def cell_test_atom_result() do
    {:ok, result} = Nock.nock(0, cell_test_atom_formula())
    assert result == 1
    result
  end

  def cell_test_cell_formula() do
    ExtNockTerms.sexpr_to_noun!({:cell_test, [{:constant, [[1, 1]]}]})
  end

  def cell_test_cell_result() do
    {:ok, result} = Nock.nock(0, cell_test_cell_formula())
    assert result == 0
    result
  end

  def cell_test_slot_formula() do
    ExtNockTerms.sexpr_to_noun!({:cell_test, [{:slot, [1]}]})
  end

  def cell_test_slot_result_atom() do
    {:ok, result} = Nock.nock(1, cell_test_slot_formula())
    assert result == 1
    result
  end

  def cell_test_slot_result_cell() do
    {:ok, result} = Nock.nock([1, 1], cell_test_slot_formula())
    assert result == 0
    result
  end

  @doc """
  I test the :incr constructor (Nock formula 4).
  """
  def incr_slot_formula() do
    ExtNockTerms.sexpr_to_noun!({:incr, [{:slot, [1]}]})
  end

  def incr_slot_result() do
    {:ok, result} = Nock.nock(1, incr_slot_formula())
    assert result == 2
    result
  end

  def incr_cell_test_formula() do
    ExtNockTerms.sexpr_to_noun!({:incr, [{:cell_test, [{:slot, [1]}]}]})
  end

  def incr_cell_test_result() do
    {:ok, result} = Nock.nock([1, 1], incr_cell_test_formula())
    assert result == 1
    result
  end

  @doc """
  I test the :eq constructor (Nock formula 5).
  """
  def eq_formula() do
    ExtNockTerms.sexpr_to_noun!(
      {:eq,
       [
         {:slot, [2]},
         {:slot, [3]}
       ]}
    )
  end

  def eq_result_equal() do
    {:ok, result} = Nock.nock([1 | 1], eq_formula())
    assert result == 0
    result
  end

  def eq_result_not_equal() do
    {:ok, result} = Nock.nock([0 | 1], eq_formula())
    assert result == 1
    result
  end

  @doc """
  I test the :ife constructor (Nock formula 6).
  """
  def ife_formula_cond_0() do
    ExtNockTerms.sexpr_to_noun!(
      {:ife,
       [
         {:constant, [0]},
         {:constant, [8]},
         {:constant, [9]}
       ]}
    )
  end

  def ife_formula_cond_0_result() do
    {:ok, result} = Nock.nock([0, 1], ife_formula_cond_0())
    assert result == 8
    result
  end

  def ife_formula_cond_1() do
    ExtNockTerms.sexpr_to_noun!(
      {:ife,
       [
         {:constant, [1]},
         {:constant, [8]},
         {:constant, [9]}
       ]}
    )
  end

  def ife_formula_cond_1_result() do
    {:ok, result} = Nock.nock([0, 1], ife_formula_cond_1())
    assert result == 9
    result
  end

  def ife_formula_eq_true() do
    ExtNockTerms.sexpr_to_noun!(
      {:ife,
       [
         {:eq, [{:constant, [1]}, {:constant, [1]}]},
         {:constant, [8]},
         {:constant, [9]}
       ]}
    )
  end

  def ife_formula_eq_true_result() do
    {:ok, result} = Nock.nock([0, 1], ife_formula_eq_true())
    assert result == 8
    result
  end

  def ife_formula_eq_false() do
    ExtNockTerms.sexpr_to_noun!(
      {:ife,
       [
         {:eq, [{:constant, [1]}, {:constant, [0]}]},
         {:constant, [8]},
         {:constant, [9]}
       ]}
    )
  end

  def ife_formula_eq_false_result() do
    {:ok, result} = Nock.nock([0, 1], ife_formula_eq_false())
    assert result == 9
    result
  end

  def ife_formula_cond_2() do
    ExtNockTerms.sexpr_to_noun!(
      {:ife,
       [
         {:constant, [2]},
         {:constant, [8]},
         {:constant, [9]}
       ]}
    )
  end

  def ife_formula_cond_2_result_error() do
    result = Nock.nock([0, 1], ife_formula_cond_2())
    assert {:error, _} = result
    result
  end

  @doc """
  I test the :compose constructor (Nock formula 7).
  """
  def compose_formula_1() do
    ExtNockTerms.sexpr_to_noun!(
      {:compose,
       [
         {:cell_test, [{:slot, [1]}]},
         {:incr, [{:slot, [1]}]}
       ]}
    )
  end

  def compose_formula_1_result() do
    {:ok, result} = Nock.nock(42, compose_formula_1())
    assert result == 2
    result
  end

  def compose_formula_2() do
    ExtNockTerms.sexpr_to_noun!(
      {:compose,
       [
         {:incr, [{:slot, [1]}]},
         {:incr, [{:slot, [1]}]}
       ]}
    )
  end

  def compose_formula_2_result() do
    {:ok, result} = Nock.nock(42, compose_formula_2())
    assert result == 44
    result
  end

  @doc """
  I test the :push constructor (Nock formula 8).
  """
  def push_formula_1() do
    ExtNockTerms.sexpr_to_noun!(
      {:push,
       [
         {:incr, [{:slot, [1]}]},
         {:slot, [1]}
       ]}
    )
  end

  def push_formula_1_result() do
    {:ok, result} = Nock.nock(42, push_formula_1())
    assert result == [43 | 42]
    result
  end

  def push_formula_2() do
    ExtNockTerms.sexpr_to_noun!(
      {:push,
       [
         {:incr, [{:slot, [1]}]},
         {:incr, [{:slot, [3]}]}
       ]}
    )
  end

  def push_formula_2_result() do
    {:ok, result} = Nock.nock(42, push_formula_2())
    assert result == 43
    result
  end

  @doc """
  I test the :invoke constructor (Nock formula 9).
  """
  def invoke_constructor_test() do
    # From Hoon `!=((|=(@ +(a)) 2))`:
    subject =
      ExtNockTerms.sexpr_to_noun!([
        [{:incr, [{:slot, [6]}]}, [0, [0, 1]]],
        {:slot, [1]}
      ])

    formula =
      ExtNockTerms.sexpr_to_noun!(
        {:invoke, [2, [[0, 4], [7, [[0, 3] | [1, 2]]] | [0, 11]]]}
      )

    {:ok, result} = Nock.nock(subject, formula)
    assert result == 3
    formula
  end

  @doc """
  I test the :replace constructor (Nock formula 10).
  """
  def replace_test_subject() do
    Noun.Format.parse_always("[10 [20 30]]")
  end

  def replace_axis_2_formula() do
    ExtNockTerms.sexpr_to_noun!(
      {:replace,
       [
         2,
         {:constant, [99]},
         {:slot, [1]}
       ]}
    )
  end

  def replace_axis_2_result() do
    {:ok, result} =
      Nock.nock(replace_test_subject(), replace_axis_2_formula())

    assert result == [99 | [20 | 30]]
    result
  end

  def replace_axis_3_formula() do
    ExtNockTerms.sexpr_to_noun!(
      {:replace,
       [
         3,
         {:constant, [88]},
         {:slot, [1]}
       ]}
    )
  end

  def replace_axis_3_result() do
    {:ok, result} =
      Nock.nock(replace_test_subject(), replace_axis_3_formula())

    assert result == [10 | 88]
    result
  end

  def replace_axis_6_formula() do
    ExtNockTerms.sexpr_to_noun!(
      {:replace,
       [
         6,
         {:constant, [77]},
         {:slot, [1]}
       ]}
    )
  end

  def replace_axis_6_result() do
    {:ok, result} =
      Nock.nock(replace_test_subject(), replace_axis_6_formula())

    assert result == [10 | [77 | 30]]
    result
  end

  def replace_axis_4_formula() do
    ExtNockTerms.sexpr_to_noun!(
      {:replace,
       [
         4,
         {:constant, [42]},
         {:slot, [1]}
       ]}
    )
  end

  def replace_axis_4_result_error() do
    result = Nock.nock(replace_test_subject(), replace_axis_4_formula())
    assert {:error, _} = result
    result
  end

  @doc """
  I test the :hint constructor (Nock formula 11).
  """
  def hint_formula_1() do
    ExtNockTerms.sexpr_to_noun!({:hint, [1, {:constant, [1]}]})
  end

  def hint_formula_1_result() do
    {:ok, result} = Nock.nock([0 | 1], hint_formula_1())
    assert result == 1
    result
  end

  def hint_formula_2() do
    ExtNockTerms.sexpr_to_noun!({:hint, [37, {:incr, [[0, 3]]}]})
  end

  def hint_formula_2_result() do
    {:ok, result} = Nock.nock([132 | 19], hint_formula_2())
    assert result == 20
    result
  end

  def hint_formula_3() do
    ExtNockTerms.sexpr_to_noun!({:hint, [[37, 1, 0], {:incr, [[0, 3]]}]})
  end

  def hint_formula_3_result() do
    {:ok, result} = Nock.nock([132 | 19], hint_formula_3())
    assert result == 20
    result
  end

  def hint_formula_error() do
    ExtNockTerms.sexpr_to_noun!({:hint, [[1, 0], {:constant, [1]}]})
  end

  def hint_formula_error_result() do
    result = Nock.nock([0 | 1], hint_formula_error())
    assert {:error, _} = result
    result
  end

  @doc """
  I test core production and activation.
  """
  def inc_formula() do
    ExtNockTerms.sexpr_to_noun!({:incr, [{:slot, [1]}]})
  end

  def inc_result() do
    {:ok, result} = Nock.nock(100, inc_formula())
    assert result == 101
    result
  end

  def dec_formula() do
    ExtNockTerms.sexpr_to_noun!([
      8,
      [1, 0],
      8,
      [1, 6, [5, [0, 7], 4, 0, 6], [0, 6], 9, 2, [0, 2], [4, 0, 6], 0, 7],
      9,
      2,
      0,
      1
    ])
  end

  def dec_result() do
    {:ok, result} = Nock.nock(100, dec_formula())
    assert result == 99
    result
  end

  def inc_core_formula() do
    ExtNockTerms.sexpr_to_noun!(
      {:create_core_1, [{:incr, [{:slot, [6]}]}, {:constant, [0]}, 1729]}
    )
  end

  def inc_core_formula_structure() do
    result = inc_core_formula()
    assert result == [[[1, 4, 0 | 6], [1 | 0], 0 | 2] | 1729]
    result
  end

  def inc_core_call_result() do
    call_formula = ExtNockTerms.sexpr_to_noun!({:call_core_1, [123]})
    {:ok, result} = Nock.nock(inc_core_formula(), call_formula)
    assert result == 124
    result
  end

  @doc """
  I test the compile_to_nock_term! function success case.
  """
  def compile_to_nock_term_success_test() do
    tvc(:slot, [tvc0({:atom, 2})])
  end

  def compile_to_nock_term_success_test_result() do
    compiled =
      ExtNockTerms.compile_to_nock_term!(compile_to_nock_term_success_test())

    expected = tvc(:cell, [tvc0({:atom, 0}), tvc0({:atom, 2})])
    assert compiled == expected
    compiled
  end
end
