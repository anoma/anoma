defmodule Examples.ENockPoly.ENockTerms do
  use Memoize

  import ExUnit.Assertions
  alias NockPoly.NockTerms

  ####################################################################
  #  Tests for NockTerms conversion and typecheck invariants using
  # nouns (Nock terms) lifted from Nock examples.
  #
  #  In these tests the nouns are created by parsing string representations
  #  (as in enock.ex and nock.ex). For each noun we:
  #    - Convert it to a nock_poly_term (via `NockTerms.from_noun/1`),
  #    - Ensure it passes typecheck,
  #    - Convert it back to a Noun.t() with `NockTerms.to_noun/1` and verify round‑trip invariance.
  #
  #  We also verify that an invalid term (manually constructed) raises an error.

  @doc """
  nock_term_test_one_two: Tests conversion round-trip using a Nock term
  lifted from the one_two examples.
  """
  def nock_term_test_one_two_noun() do
    {:ok, noun} = Noun.Format.parse("[1 2]")
    noun
  end

  def nock_term_test_one_two() do
    NockTerms.from_noun(nock_term_test_one_two_noun())
  end

  def nock_term_test_one_two_typecheck() do
    result = NockTerms.typecheck(nock_term_test_one_two())
    assert result == :ok
    result
  end

  def nock_term_test_one_two_round_trip() do
    result = NockTerms.to_noun(nock_term_test_one_two())
    assert result == nock_term_test_one_two_noun()
    assert NockTerms.from_noun(result) == nock_term_test_one_two()
    result
  end

  @doc """
  nock_term_test_indexed: Tests conversion round-trip using a Nock term
  lifted from the indexed_noun examples.
  """
  def nock_term_test_indexed_noun() do
    {:ok, noun} = Noun.Format.parse("[[4 5] [12 13] 7]")
    noun
  end

  def nock_term_test_indexed() do
    NockTerms.from_noun(nock_term_test_indexed_noun())
  end

  def nock_term_test_indexed_typecheck() do
    result = NockTerms.typecheck(nock_term_test_indexed())
    assert result == :ok
    result
  end

  def nock_term_test_indexed_round_trip() do
    result = NockTerms.to_noun(nock_term_test_indexed())
    assert result == nock_term_test_indexed_noun()
    assert NockTerms.from_noun(result) == nock_term_test_indexed()
    result
  end

  @doc """
  nock_term_test_counter_arm: Tests conversion round-trip using a Nock term
  lifted from the counter_arm examples.
  """
  def nock_term_test_counter_arm_noun() do
    counter_arm = """
    [ 6
      [5 [1 1] 8 [9 1.406 0 1.023] 9 2 10 [6 0 118] 0 2]
      [6 [5 [1 1] 8 [9 1.406 0 1.023] 9 2 10 [6 0 238] 0 2] [6 [5 [1 1] 8 [9 1.406 0 1.023] 9 2 10 [6 0 958] 0 2] [6 [5 [1 0] 0 446] [0 0] 6 [0 3.570] [1 0] 1 1] 1 1] 1 1]
      1
      1
    ]
    """

    Noun.Format.parse_always(counter_arm)
  end

  def nock_term_test_counter_arm() do
    NockTerms.from_noun(nock_term_test_counter_arm_noun())
  end

  def nock_term_test_counter_arm_typecheck() do
    result = NockTerms.typecheck(nock_term_test_counter_arm())
    assert result == :ok
    result
  end

  def nock_term_test_counter_arm_round_trip() do
    result = NockTerms.to_noun(nock_term_test_counter_arm())
    assert result == nock_term_test_counter_arm_noun()
    assert NockTerms.from_noun(result) == nock_term_test_counter_arm()
    result
  end

  @doc """
  nock_term_test_invalid: Checks that an invalid term raises an error.
  (This term is invalid because a cell should have two children, and
  this alleged cell has none.)
  """
  def nock_term_test_invalid() do
    alias NockPoly.Term, as: T
    # Create an invalid cell (missing children) but with the proper structure
    res = T.com_tv(:cell, [])

    assert_raise CaseClauseError, fn ->
      NockTerms.to_noun(res)
    end

    res
  end
end
