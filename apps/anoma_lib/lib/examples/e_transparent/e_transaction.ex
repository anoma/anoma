defmodule Examples.ETransparent.ETransaction do
  alias Examples.ETransparent.EAction

  alias Anoma.TransparentResource.Transaction

  use TestHelper.TestMacro

  def empty() do
    res = %Transaction{}

    assert Transaction.verify(res)
    assert Transaction.compose(res, res) == res

    res
  end

  def single_swap() do
    res = %{empty() | actions: MapSet.new([EAction.trivial_swap_action()])}

    assert Transaction.verify(res)
    assert Transaction.compose(res, res) == res

    res
  end

  def single_swap_invalid_delta() do
    res = %Transaction{single_swap() | delta: %{<<1>> => 1}}

    assert {:error, _} = Transaction.verify(res)

    res
  end

  def invalid_swap() do
    actions =
      MapSet.new([EAction.trivial_action_proofs_missing_nullifier_proof()])

    res = %Transaction{empty() | actions: actions}

    assert {:error, _} = Transaction.verify(res)

    res
  end
end
