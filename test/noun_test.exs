defmodule AnomaTest.Noun do
  use ExUnit.Case, async: true

  import Noun
  alias Noun.Format

  doctest(Noun)
  doctest(Noun.Format)

  @testing_noun Noun.Format.parse_always("[[4 5] [12 13] 7]")
  def testing_noun() do
    @testing_noun
  end

  test "parse sensible terms" do
    assert {:ok, [1 | 2]} = Noun.Format.parse("[1 2]")
    assert {:ok, [1, [3 | 5] | 2]} = Noun.Format.parse("[1 [[3 5] 2]]")
  end

  test "don't parse incorrect terms" do
    assert :error = Noun.Format.parse("[[[[1]]]]")
    assert :error = Noun.Format.parse("[[[[")
    assert :error = Noun.Format.parse("]]]]")
  end

  test "indexing" do
    assert axis(1, testing_noun()) == {:ok, testing_noun()}
    assert axis(2, testing_noun()) == {:ok, [4 | 5]}
    assert axis(6, testing_noun()) == {:ok, [12 | 13]}
    assert axis(7, testing_noun()) == {:ok, 7}
    assert axis(4, testing_noun()) == {:ok, 4}
    assert axis(5, testing_noun()) == {:ok, 5}
    assert axis(12, testing_noun()) == {:ok, 12}
    assert axis(13, testing_noun()) == {:ok, 13}
  end

  test "inserting" do
    assert {:ok, term} = replace(2, 2, testing_noun())
    assert axis(2, term) == {:ok, 2}
  end
end
