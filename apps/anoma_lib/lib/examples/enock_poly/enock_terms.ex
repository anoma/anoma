defmodule Examples.ENockPoly.ENockTerms do
  use Memoize

  import ExUnit.Assertions
  alias NockPoly.NockTerms
  alias Noun

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
  def nock_term_test_one_two() do
    {:ok, noun_one_two} = Noun.Format.parse("[1 2]")
    res = NockTerms.from_noun(noun_one_two)
    assert NockTerms.typecheck(res) == :ok
    rt = NockTerms.to_noun(res)
    assert rt == noun_one_two
    assert NockTerms.from_noun(rt) == res
    res
  end

  @doc """
  nock_term_test_indexed: Tests conversion round-trip using a Nock term
  lifted from the indexed_noun examples.
  """
  def nock_term_test_indexed() do
    {:ok, noun_indexed} = Noun.Format.parse("[[4 5] [12 13] 7]")
    res = NockTerms.from_noun(noun_indexed)
    assert NockTerms.typecheck(res) == :ok
    rt = NockTerms.to_noun(res)
    assert rt == noun_indexed
    assert NockTerms.from_noun(rt) == res
    res
  end

  @doc """
  nock_term_test_counter_arm: Tests conversion round-trip using a Nock term
  lifted from the counter_arm examples.
  """
  def nock_term_test_counter_arm() do
    counter_arm = """
    [ 6
      [5 [1 1] 8 [9 1.406 0 1.023] 9 2 10 [6 0 118] 0 2]
      [6 [5 [1 1] 8 [9 1.406 0 1.023] 9 2 10 [6 0 238] 0 2] [6 [5 [1 1] 8 [9 1.406 0 1.023] 9 2 10 [6 0 958] 0 2] [6 [5 [1 0] 0 446] [0 0] 6 [0 3.570] [1 0] 1 1] 1 1] 1 1]
      1
      1
    ]
    """

    noun_counter = Noun.Format.parse_always(counter_arm)
    res = NockTerms.from_noun(noun_counter)
    assert NockTerms.typecheck(res) == :ok
    rt = NockTerms.to_noun(res)
    assert rt == noun_counter
    assert NockTerms.from_noun(rt) == res
    res
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
