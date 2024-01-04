defmodule AnomaTest.Logic do
  use ExUnit.Case, async: true

  alias Anoma.Logic.{Lt, Branch, Mul, Add, Inc, Neg}

  alias Anoma.Eval

  doctest(Anoma.Logic)

  def blank(), do: {:in, [], []}

  test "Number Works" do
    assert Eval.apply(2, blank()) == 2
  end

  test "Inc Works" do
    assert Eval.apply(%Inc{in1: 1}, blank()) == 2
  end

  test "lt Works" do
    assert Eval.apply(%Lt{in1: 1, in2: 2}, blank()) == 0
    assert Eval.apply(%Lt{in1: 2, in2: 1}, blank()) == 1
  end

  test "Add Works" do
    assert Eval.apply(%Inc{in1: %Add{in1: 3, in2: 5}}, blank()) == 9
  end

  test "Mul Works" do
    assert Eval.apply(%Inc{in1: %Mul{in1: 3, in2: 5}}, blank()) == 16
  end

  test "Neg Works" do
    mul = %Mul{in1: 3, in2: 5}
    e = blank()
    assert Eval.apply(%Neg{in1: mul}, e) == -15
    assert Eval.apply(%Neg{in1: %Neg{in1: mul}}, e) == Eval.apply(mul, e)
  end

  test "Branch Works" do
    lt = %Lt{in1: 0, in2: 1}
    gt = %Lt{in1: 1, in2: 0}
    mul = %Mul{in1: 3, in2: 5}
    assert Eval.apply(%Branch{in1: 0, ip1: mul, ip2: 3}, blank()) == 15
    assert Eval.apply(%Branch{in1: 1, ip1: mul, ip2: 3}, blank()) == 3
    assert Eval.apply(%Branch{in1: lt, ip1: mul, ip2: 3}, blank()) == 15
    assert Eval.apply(%Branch{in1: gt, ip1: mul, ip2: 3}, blank()) == 3
  end
end
