defmodule Examples.ENockPoly do
  use Memoize

  require ExUnit.Assertions
  import ExUnit.Assertions
  import NockPoly
  alias NockPoly.Term, as: Term

  use TypedStruct

  @doc """
  I test `Term.depth/1` and `Term.size/1` (and thus indirectly also
  `Term.eval/3`) using a variety of terms, both closed and open.
  """
  @spec term_tests() :: bool()
  def term_tests() do
    # Closed term test cases (no variables)
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

    # An example of a direct call to `Term.cata/2`, since `depth` and
    # `size` are defined in terms of `eval` and thus do not directly
    # test the `cata` wrapper.  This also serves as an example of
    # providing a custom algebra to `cata` (the algebra parameter to
    # `eval` has the same signature, so it is also an example of that).
    # In this example we use a string constructor type and concatenate
    # all the constructor names.
    t8 = {"root", [{"left", []}, {"right", []}]}

    algebra = fn {ctor, children} ->
      to_string(ctor) <> Enum.join(children, "")
    end

    assert Term.cata(t8, algebra) == "rootleftright"

    # Open term test cases (terms with variables)

    # A single variable (open term). By definition, its depth and size are 0.
    v1 = "x"
    assert Term.depth(v1) == 0
    assert Term.size(v1) == 0

    # An open term with a single variable as a child.
    t5 = {:a, ["x"]}

    # The root is a constructor (depth = 1), and the single child is a variable (depth 0).
    assert Term.depth(t5) == 1
    # Size is 1 (the constructor) + 0 for the variable.
    assert Term.size(t5) == 1

    # An open term with multiple variables.
    t6 = {:b, ["x", {:c, ["y", "z"]}]}
    # For t6:
    #   - the variable "x" has depth 0;
    #   - {:c, ["y", "z"]} has depth = 1 + max(0, 0) = 1.
    # So overall depth = 1 + max(0, 1) = 2.
    # The sizes are: "x" → 0; {:c, ["y", "z"]} → 1 + (0 + 0) = 1;
    # so overall size = 1 (for :b) + (0 + 1) = 2.
    assert Term.depth(t6) == 2
    assert Term.size(t6) == 2

    # An open term with no variables (a closed term, but viewed as open).
    t7 = {:d, [{:e, []}, {:f, []}]}
    # depth = 1 + max(1,1)
    assert Term.depth(t7) == 2
    # size = 1 + (1 + 1)
    assert Term.size(t7) == 3

    true
  end
end
