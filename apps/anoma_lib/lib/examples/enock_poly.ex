defmodule Examples.ENockPoly do
  use Memoize

  require ExUnit.Assertions
  import ExUnit.Assertions
  import NockPoly
  alias NockPoly.Term, as: Term

  use TypedStruct

  @doc """
  I test `Term.depth/1` and `Term.size/1` (and thus indirectly also
  `Term.cata/2`) using a variety of terms.
  """
  @spec term_tests() :: bool()
  def term_tests() do
    t1 = {1, []}
    assert Term.depth(t1) == 1
    assert Term.size(t1) == 1

    t2 = {:a, [{:a, []}, {:b, []}]}
    assert Term.depth(t2) == 2
    assert Term.size(t2) == 3

    t3 = {:x, [{:y, [{:z, []}]}]}
    assert Term.depth(t3) == 3
    assert Term.size(t3) == 3

    t4 = {0, [{1, []}, {2, [{3, []}, {4, []}]}, {5, []}]}
    assert Term.depth(t4) == 3
    assert Term.size(t4) == 6

    true
  end
end
