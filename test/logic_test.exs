defmodule AnomaTest.Logic do
  use ExUnit.Case, async: true

  alias Anoma.Logic.{Lt, Branch, Mul, Add, Inc, Neg}

  alias Anoma.{Eval, PartialTx}

  doctest(Anoma.Logic)

  test "Number Works" do
    assert Eval.apply(2, PartialTx.empty()) == 2
  end

  test "Inc Works" do
    assert Eval.apply(%Inc{in1: 1}, PartialTx.empty()) == 2
  end

  test "lt Works" do
    assert Eval.apply(%Lt{in1: 1, in2: 2}, PartialTx.empty()) == 0
    assert Eval.apply(%Lt{in1: 2, in2: 1}, PartialTx.empty()) == 1
  end

  test "Add Works" do
    assert Eval.apply(%Inc{in1: %Add{in1: 3, in2: 5}}, PartialTx.empty()) == 9
  end

  test "Mul Works" do
    assert Eval.apply(%Inc{in1: %Mul{in1: 3, in2: 5}}, PartialTx.empty()) == 16
  end

  test "Neg Works" do
    mul = %Mul{in1: 3, in2: 5}
    e = PartialTx.empty()
    assert Eval.apply(%Neg{in1: mul}, e) == -15
    assert Eval.apply(%Neg{in1: %Neg{in1: mul}}, e) == Eval.apply(mul, e)
  end

  test "Branch Works" do
    lt = %Lt{in1: 0, in2: 1}
    gt = %Lt{in1: 1, in2: 0}
    mul = %Mul{in1: 3, in2: 5}
    assert Eval.apply(%Branch{in1: 0, ip1: mul, ip2: 3}, PartialTx.empty()) == 15
    assert Eval.apply(%Branch{in1: 1, ip1: mul, ip2: 3}, PartialTx.empty()) == 3
    assert Eval.apply(%Branch{in1: lt, ip1: mul, ip2: 3}, PartialTx.empty()) == 15
    assert Eval.apply(%Branch{in1: gt, ip1: mul, ip2: 3}, PartialTx.empty()) == 3
  end
end
