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
    assert term == {{:atom, val}, []}
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
    assert term == val
    term
  end

  @doc """
  I test conversion from the sexpr representation to open_nock_poly_term
  for a cell.
  """
  def nock_poly_sexpr_cell_test() do
    sexpr = [1, 2]
    term = ExtNockTerms.from_sexpr!(sexpr)
    expected = {:cell, [{{:atom, 1}, []}, {{:atom, 2}, []}]}
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
    sexpr = [[4, 5], [12, 13], 7]
    term = ExtNockTerms.from_sexpr!(sexpr)
    expected_inner1 = {:cell, [{{:atom, 4}, []}, {{:atom, 5}, []}]}
    expected_inner2 = {:cell, [{{:atom, 12}, []}, {{:atom, 13}, []}]}

    expected =
      {:cell, [expected_inner1, {:cell, [expected_inner2, {{:atom, 7}, []}]}]}

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
    sexpr = [{:var, 7}, 99]
    term = ExtNockTerms.from_sexpr!(sexpr)
    expected = {:cell, [7, {{:atom, 99}, []}]}
    assert term == expected

    closed_term =
      ExtNockTerms.substitute(term, fn v -> {{:atom, v * 10}, []} end)

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
    malformed_term = {:slot, []}

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
    malformed_term = {:constant, []}

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
  I test the compile_to_nock_term! function success case.
  """
  def compile_to_nock_term_success_test() do
    # Create a simple extended term
    term = {:slot, [{{:atom, 2}, []}]}

    # Test successful compilation
    compiled = ExtNockTerms.compile_to_nock_term!(term)

    # Verify the structure
    expected = {:cell, [{{:atom, 0}, []}, {{:atom, 2}, []}]}
    assert compiled == expected

    term
  end
end
