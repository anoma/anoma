defmodule Examples.ETransparent.ETransaction do
  alias Examples.ETransparent.EAction

  alias Anoma.RM.Transparent.Transaction

  use TestHelper.TestMacro

  @spec empty() :: Transaction.t()
  def empty() do
    res = %Transaction{}

    assert Transaction.verify(res)
    assert Transaction.compose(res, res) == res
    res
  end

  @spec single_swap() :: Transaction.t()
  def single_swap() do
    res = %{empty() | actions: MapSet.new([EAction.trivial_swap_action()])}

    assert Transaction.verify(res)
    assert Transaction.compose(res, res) == res

    res
  end

  @spec commit_intent() :: Transaction.t()
  def commit_intent() do
    actions =
      MapSet.new([EAction.trivial_true_commit_action()])

    res1 = %Transaction{empty() | actions: actions}
    # unbalanced
    refute Transaction.verify(res1)
    res1
  end

  @spec nullify_intent_eph() :: Transaction.t()
  def nullify_intent_eph() do
    actions =
      MapSet.new([EAction.trivial_true_eph_nullifier_action()])

    res1 = %Transaction{empty() | actions: actions}

    # unbalanced
    refute Transaction.verify(res1)

    res1
  end

  @spec nullify_intent() :: Transaction.t()
  def nullify_intent() do
    actions =
      MapSet.new([EAction.trivial_true_2_nullifier_action()])

    res1 = %Transaction{empty() | actions: actions}

    # unbalanced
    refute Transaction.verify(res1)

    res1
  end

  @spec swap_from_actions() :: Transaction.t()
  def swap_from_actions() do
    res = Transaction.compose(nullify_intent_eph(), commit_intent())

    assert 2 == Transaction.delta(res)
    assert Transaction.verify(res)

    res
  end

  # This should fail verification on submission if storage is not
  # primed
  @spec swap_from_actions_non_eph_nullifier() :: Transaction.t()
  def swap_from_actions_non_eph_nullifier() do
    res = Transaction.compose(nullify_intent(), commit_intent())

    assert 2 = Transaction.delta(res)
    assert Transaction.verify(res)

    res
  end
end
