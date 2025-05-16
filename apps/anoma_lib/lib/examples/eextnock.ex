defmodule Examples.EExtNock do
  @moduledoc """
  I include examples and tests for Nock macros.

  Many of the tests in this module are based on test cases from example
  Nock interpreters available on the Urbit website, particularly the JavaScript
  reference implementation.
  """

  import ExtNock
  alias ExtNock.ExtNockTerms
  use TypedStruct
  require ExUnit.Assertions
  import ExUnit.Assertions

  @doc """
  I test conversion from the sexpr representation to open_nock_poly_term
  for a lone atom.
  """
  def nock_poly_sexpr_atom_test() do
    val = 42
    term = ExtNockTerms.from_sexpr!(val)
    assert term == NockPoly.Term.com_tv({:atom, val}, [])
    term
  end

  @doc """
  I test conversion from the sexpr representation to open_nock_poly_term for a
  lone variable.
  """
  def nock_poly_sexpr_variable_test() do
    val = 7
    var = {:var, val}
    term = ExtNockTerms.from_sexpr!(var)
    assert term == NockPoly.Term.var_tv(val)
    term
  end

  @doc """
  I test conversion from the sexpr representation to open_nock_poly_term
  for a cell.
  """
  def nock_poly_sexpr_cell_test() do
    sexpr = [1, 2]
    term = ExtNockTerms.from_sexpr!(sexpr)

    expected =
      NockPoly.Term.com_tv(:cell, [
        NockPoly.Term.com_tv({:atom, 1}, []),
        NockPoly.Term.com_tv({:atom, 2}, [])
      ])

    assert term == expected
    expected_noun = Noun.Format.parse_always("[1 2]")
    assert ExtNockTerms.to_noun!(term) == expected_noun
    term
  end

  @doc """
  I test conversion from the sexpr representation to open_nock_poly_term for a
  nested expression.
  """
  def nock_poly_sexpr_nested_test() do
    alias NockPoly.Term, as: T
    sexpr = [[4, 5], [12, 13], 7]
    term = ExtNockTerms.from_sexpr!(sexpr)

    expected_inner1 =
      T.com_tv(:cell, [
        T.com_tv({:atom, 4}, []),
        T.com_tv({:atom, 5}, [])
      ])

    expected_inner2 =
      T.com_tv(:cell, [
        T.com_tv({:atom, 12}, []),
        T.com_tv({:atom, 13}, [])
      ])

    expected =
      T.com_tv(:cell, [
        expected_inner1,
        T.com_tv(:cell, [
          expected_inner2,
          T.com_tv({:atom, 7}, [])
        ])
      ])

    assert term == expected
    expected_noun = Noun.Format.parse_always("[[4 5] [12 13] 7]")
    assert ExtNockTerms.to_noun!(term) == expected_noun
    term
  end

  @doc """
  I test conversion from the sexpr representation to open_nock_poly_term for a
  term with variables.
  """
  def nock_poly_sexpr_with_variables_test() do
    alias NockPoly.Term, as: T
    sexpr = [{:var, 7}, 99]
    term = ExtNockTerms.from_sexpr!(sexpr)
    expected = T.com_tv(:cell, [T.var_tv(7), T.com_tv({:atom, 99}, [])])
    assert term == expected

    closed_term =
      ExtNockTerms.substitute(term, fn v -> T.com_tv({:atom, v * 10}, []) end)

    expected_noun = Noun.Format.parse_always("[70 99]")
    assert ExtNockTerms.to_noun!(closed_term) == expected_noun
    term
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
  def slot_constructor_test() do
    nested_subject = Noun.Format.parse_always("[[2 0] [3 1]]")

    # Test slot 1 (whole subject)
    slot1_formula = ExtNockTerms.sexpr_to_noun!({:slot, [1]})
    {:ok, result1} = Nock.nock(nested_subject, slot1_formula)
    assert result1 == nested_subject

    # Test slot 2 (head of subject)
    slot2_formula = ExtNockTerms.sexpr_to_noun!({:slot, [2]})
    {:ok, result2} = Nock.nock(nested_subject, slot2_formula)
    assert result2 == [2 | 0]

    # Test slot 3 (tail of subject)
    slot3_formula = ExtNockTerms.sexpr_to_noun!({:slot, [3]})
    {:ok, result3} = Nock.nock(nested_subject, slot3_formula)
    assert result3 == [3 | 1]

    # Test slot 4 (head of head)
    slot4_formula = ExtNockTerms.sexpr_to_noun!({:slot, [4]})
    {:ok, result4} = Nock.nock(nested_subject, slot4_formula)
    assert result4 == 2

    # Test slot 5 (tail of head)
    slot5_formula = ExtNockTerms.sexpr_to_noun!({:slot, [5]})
    {:ok, result5} = Nock.nock(nested_subject, slot5_formula)
    assert result5 == 0

    # Test slot 6 (head of tail)
    slot6_formula = ExtNockTerms.sexpr_to_noun!({:slot, [6]})
    {:ok, result6} = Nock.nock(nested_subject, slot6_formula)
    assert result6 == 3

    # Test slot 7 (tail of tail)
    slot7_formula = ExtNockTerms.sexpr_to_noun!({:slot, [7]})
    {:ok, result7} = Nock.nock(nested_subject, slot7_formula)
    assert result7 == 1

    # Test invalid slot (should raise error)
    slot8_formula = ExtNockTerms.sexpr_to_noun!({:slot, [8]})
    assert Nock.nock(nested_subject, slot8_formula) == :error

    {:slot, [1]}
  end

  @doc """
  I test the :constant constructor which compiles to Nock formula 1 (constant).
  """
  def constant_constructor_test() do
    # Atom constant
    const_formula = ExtNockTerms.sexpr_to_noun!({:constant, [42]})

    # Different subjects - should always return the constant
    {:ok, result1} = Nock.nock(0, const_formula)
    assert result1 == 42

    {:ok, result2} = Nock.nock([1, 2], const_formula)
    assert result2 == 42

    # Cell constant
    cell_const_formula = ExtNockTerms.sexpr_to_noun!({:constant, [[1, 2]]})

    {:ok, cell_result} = Nock.nock(0, cell_const_formula)
    assert cell_result == [1 | 2]

    {:constant, [42]}
  end

  @doc """
  I test error cases for the :slot constructor.

  I verify that errors are properly detected for various invalid inputs
  and operations on malformed slot terms.
  """
  def slot_constructor_error_test() do
    alias NockPoly.Term, as: T
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
    malformed_term = T.in_tv({:tcom, {:slot, []}})

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
    alias NockPoly.Term, as: T
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
    malformed_term = T.in_tv({:tcom, {:constant, []}})

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
  def evaluate_constructor_test() do
    # Test case 1: f.evaluate(1, [[1, 2], [1, [0, 1]]]) => 2
    eval_formula1 =
      ExtNockTerms.sexpr_to_noun!(
        {:evaluate,
         [
           {:constant, [2]},
           {:constant, [{:slot, [1]}]}
         ]}
      )

    {:ok, result1} = Nock.nock(1, eval_formula1)
    assert result1 == 2

    # Test case 2: f.evaluate(77, [[1, 42], [1, [1, 153]]]) => 153
    eval_formula2 =
      ExtNockTerms.sexpr_to_noun!(
        {:evaluate,
         [
           {:constant, [42]},
           {:constant, [{:constant, [153]}]}
         ]}
      )

    {:ok, result2} = Nock.nock(77, eval_formula2)
    assert result2 == 153

    # Return a value for the example test system
    {:evaluate,
     [
       {:constant, [2]},
       {:constant, [{:slot, [1]}]}
     ]}
  end

  @doc """
  I test the :cell_test constructor (Nock formula 3).
  """
  def cell_test_constructor_test() do
    cell_test_atom1 =
      ExtNockTerms.sexpr_to_noun!({:cell_test, [{:constant, [1]}]})

    {:ok, result_atom1} = Nock.nock(0, cell_test_atom1)
    assert result_atom1 == 1

    cell_test_cell1 =
      ExtNockTerms.sexpr_to_noun!({:cell_test, [{:constant, [[1, 1]]}]})

    {:ok, result_cell1} = Nock.nock(0, cell_test_cell1)
    # 0 means is a cell
    assert result_cell1 == 0

    cell_test_slot =
      ExtNockTerms.sexpr_to_noun!({:cell_test, [{:slot, [1]}]})

    {:ok, result_slot_atom} = Nock.nock(1, cell_test_slot)
    assert result_slot_atom == 1

    {:ok, result_slot_cell} = Nock.nock([1, 1], cell_test_slot)
    assert result_slot_cell == 0

    cell_test_slot
  end

  @doc """
  I test the :incr constructor (Nock formula 4).
  """
  def incr_constructor_test() do
    incr_formula1 =
      ExtNockTerms.sexpr_to_noun!({:incr, [{:slot, [1]}]})

    {:ok, result1} = Nock.nock(1, incr_formula1)
    assert result1 == 2

    incr_formula2 =
      ExtNockTerms.sexpr_to_noun!({:incr, [{:cell_test, [{:slot, [1]}]}]})

    {:ok, result2} = Nock.nock([1, 1], incr_formula2)
    assert result2 == 1

    incr_formula1
  end

  @doc """
  I test the :eq constructor (Nock formula 5).
  """
  def eq_constructor_test() do
    eq_formula =
      ExtNockTerms.sexpr_to_noun!(
        {:eq,
         [
           {:slot, [2]},
           {:slot, [3]}
         ]}
      )

    {:ok, result1} = Nock.nock([1 | 1], eq_formula)
    assert result1 == 0

    {:ok, result2} = Nock.nock([0 | 1], eq_formula)
    assert result2 == 1

    eq_formula
  end

  @doc """
  I test the :ife constructor (Nock formula 6).
  """
  def ife_constructor_test() do
    ife_formula1 =
      ExtNockTerms.sexpr_to_noun!(
        {:ife,
         [
           {:constant, [0]},
           {:constant, [8]},
           {:constant, [9]}
         ]}
      )

    {:ok, result1} = Nock.nock([0, 1], ife_formula1)
    assert result1 == 8

    ife_formula2 =
      ExtNockTerms.sexpr_to_noun!(
        {:ife,
         [
           {:constant, [1]},
           {:constant, [8]},
           {:constant, [9]}
         ]}
      )

    {:ok, result2} = Nock.nock([0, 1], ife_formula2)
    assert result2 == 9

    ife_formula3 =
      ExtNockTerms.sexpr_to_noun!(
        {:ife,
         [
           {:eq, [{:constant, [1]}, {:constant, [1]}]},
           {:constant, [8]},
           {:constant, [9]}
         ]}
      )

    {:ok, result3} = Nock.nock([0, 1], ife_formula3)
    assert result3 == 8

    ife_formula4 =
      ExtNockTerms.sexpr_to_noun!(
        {:ife,
         [
           {:eq, [{:constant, [1]}, {:constant, [0]}]},
           {:constant, [8]},
           {:constant, [9]}
         ]}
      )

    {:ok, result4} = Nock.nock([0, 1], ife_formula4)
    assert result4 == 9

    # Error case: condition is 2 (neither 0 nor 1)
    ife_formula5 =
      ExtNockTerms.sexpr_to_noun!(
        {:ife,
         [
           {:constant, [2]},
           {:constant, [8]},
           {:constant, [9]}
         ]}
      )

    assert Nock.nock([0, 1], ife_formula5) == :error

    ife_formula1
  end

  @doc """
  I test the :compose constructor (Nock formula 7).
  """
  def compose_constructor_test() do
    compose_formula1 =
      ExtNockTerms.sexpr_to_noun!(
        {:compose,
         [
           {:cell_test, [{:slot, [1]}]},
           {:incr, [{:slot, [1]}]}
         ]}
      )

    {:ok, result1} = Nock.nock(42, compose_formula1)
    assert result1 == 2

    compose_formula2 =
      ExtNockTerms.sexpr_to_noun!(
        {:compose,
         [
           {:incr, [{:slot, [1]}]},
           {:incr, [{:slot, [1]}]}
         ]}
      )

    {:ok, result2} = Nock.nock(42, compose_formula2)
    assert result2 == 44

    compose_formula1
  end

  @doc """
  I test the :push constructor (Nock formula 8).
  """
  def push_constructor_test() do
    push_formula1 =
      ExtNockTerms.sexpr_to_noun!(
        {:push,
         [
           {:incr, [{:slot, [1]}]},
           {:slot, [1]}
         ]}
      )

    {:ok, result1} = Nock.nock(42, push_formula1)
    assert result1 == [43 | 42]

    push_formula2 =
      ExtNockTerms.sexpr_to_noun!(
        {:push,
         [
           {:incr, [{:slot, [1]}]},
           {:incr, [{:slot, [3]}]}
         ]}
      )

    {:ok, result2} = Nock.nock(42, push_formula2)
    assert result2 == 43

    push_formula1
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
  def replace_constructor_test() do
    # Create a subject for our tests: [10 [20 30]]
    subject = Noun.Format.parse_always("[10 [20 30]]")

    # Replace at axis 2 (head) of the subject with value 99
    replace1 =
      ExtNockTerms.sexpr_to_noun!(
        {:replace,
         [
           2,
           {:constant, [99]},
           {:slot, [1]}
         ]}
      )

    {:ok, result1} = Nock.nock(subject, replace1)
    assert result1 == [99 | [20 | 30]]

    # Replace at axis 3 (tail) of the subject with value 88
    replace2 =
      ExtNockTerms.sexpr_to_noun!(
        {:replace,
         [
           3,
           {:constant, [88]},
           {:slot, [1]}
         ]}
      )

    {:ok, result2} = Nock.nock(subject, replace2)
    assert result2 == [10 | 88]

    # Replace at axis 6 (head of tail) of the subject with value 77
    replace3 =
      ExtNockTerms.sexpr_to_noun!(
        {:replace,
         [
           6,
           {:constant, [77]},
           {:slot, [1]}
         ]}
      )

    {:ok, result3} = Nock.nock(subject, replace3)
    assert result3 == [10 | [77 | 30]]

    # Replace at axis 4 (head of head) of the subject with value 42
    # (we shall give it a subject with no such axis)
    replace4 =
      ExtNockTerms.sexpr_to_noun!(
        {:replace,
         [
           4,
           {:constant, [42]},
           {:slot, [1]}
         ]}
      )

    assert Nock.nock(subject, replace4) == :error

    replace1
  end

  @doc """
  I test the :hint constructor (Nock formula 11).
  """
  def hint_constructor_test() do
    {:ok, result1} =
      Nock.nock(
        [0 | 1],
        ExtNockTerms.sexpr_to_noun!({:hint, [1, {:constant, [1]}]})
      )

    assert result1 == 1

    {:ok, result2} =
      Nock.nock(
        [132 | 19],
        ExtNockTerms.sexpr_to_noun!({:hint, [37, {:incr, [[0, 3]]}]})
      )

    assert result2 == 20

    {:ok, result3} =
      Nock.nock(
        [132 | 19],
        ExtNockTerms.sexpr_to_noun!({:hint, [[37, 1, 0], {:incr, [[0, 3]]}]})
      )

    assert result3 == 20

    result4 =
      Nock.nock(
        [0 | 1],
        ExtNockTerms.sexpr_to_noun!({:hint, [[1, 0], {:constant, [1]}]})
      )

    assert result4 == :error
  end

  @doc """
  I test core production and activation.
  """
  def core_production_activation_test() do
    inc_sexpr = {:incr, [{:slot, [1]}]}
    inc_formula = ExtNockTerms.sexpr_to_noun!(inc_sexpr)
    {:ok, inc_result} = Nock.nock(100, inc_formula)
    assert inc_result == 101

    # The decrement formula from
    # https://docs.urbit.org/language/nock/examples/decrement .
    dec_sexpr = [
      8,
      [1, 0],
      8,
      [1, 6, [5, [0, 7], 4, 0, 6], [0, 6], 9, 2, [0, 2], [4, 0, 6], 0, 7],
      9,
      2,
      0,
      1
    ]

    dec_formula =
      ExtNockTerms.sexpr_to_noun!(dec_sexpr)

    {:ok, dec_result} = Nock.nock(100, dec_formula)
    assert dec_result == 99

    # Commonly, the payload will be a library or chain of libraries.
    # In the following example we don't use it, so we provide some
    # random* payload.
    inc_core_payload = 1729
    inc_core_default_arg = {:constant, [0]}

    inc_core_sexpr =
      {:create_core_1,
       [{:incr, [{:slot, [6]}]}, inc_core_default_arg, inc_core_payload]}

    inc_core_formula = ExtNockTerms.sexpr_to_noun!(inc_core_sexpr)

    assert inc_core_formula == [
             [[1, 4, 0 | 6], [1 | 0], 0 | 2] | inc_core_payload
           ]

    inc_call_arg = 123

    inc_call_formula =
      ExtNockTerms.sexpr_to_noun!({:call_core_1, [inc_call_arg]})

    {:ok, inc_core_result} = Nock.nock(inc_core_formula, inc_call_formula)
    assert inc_core_result == inc_call_arg + 1
  end

  @doc """
  I test the compile_to_nock_term! function success case.
  """
  def compile_to_nock_term_success_test() do
    alias NockPoly.Term, as: T

    # Create a simple extended term
    term = T.com_tv(:slot, [T.com_tv({:atom, 2}, [])])

    # Test successful compilation
    compiled = ExtNockTerms.compile_to_nock_term!(term)

    # Verify the structure
    expected =
      T.com_tv(:cell, [T.com_tv({:atom, 0}, []), T.com_tv({:atom, 2}, [])])

    assert compiled == expected

    term
  end
end
