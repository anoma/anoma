defmodule Examples.ETransparent.ETransaction do
  alias Examples.ETransparent.EAction

  alias Anoma.TransparentResource.Transaction

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

  @spec single_swap_invalid_delta() :: Transaction.t()
  def single_swap_invalid_delta() do
    res = %Transaction{single_swap() | delta: %{<<1>> => 1}}

    assert {:error, _} = Transaction.verify(res)

    res
  end

  @spec invalid_swap() :: Transaction.t()
  def invalid_swap() do
    actions =
      MapSet.new([EAction.trivial_action_proofs_missing_nullifier_proof()])

    res = %Transaction{empty() | actions: actions}

    assert {:error, _} = Transaction.verify(res)

    res
  end

  @spec commit_intent() :: Transaction.t()
  def commit_intent() do
    actions =
      MapSet.new([EAction.trivial_true_commit_action()])

    delta = EAction.trivial_true_commit_delta()
    res = %Transaction{empty() | actions: actions, delta: delta}

    assert {:error, _} = Transaction.verify(res)

    res
  end

  @spec nullify_intent_eph() :: Transaction.t()
  def nullify_intent_eph() do
    actions =
      MapSet.new([EAction.trivial_true_eph_nullifier_action()])

    delta = EAction.trivial_true_2_nullifier_delta()
    res = %Transaction{empty() | actions: actions, delta: delta}

    assert {:error, _} = Transaction.verify(res)

    # the delta should be negative let's say we can jam and cue this
    noun = res |> Noun.Nounable.to_noun()

    assert noun |> Noun.Jam.jam() |> Noun.Jam.cue()
    assert Transaction.from_noun(noun) == {:ok, res}

    res
  end

  @spec nullify_intent_eph_rand() :: Transaction.t()
  def nullify_intent_eph_rand() do
    actions =
      MapSet.new([EAction.trivial_true_eph_nullifier_action_rand()])

    delta = EAction.trivial_true_2_nullifier_delta()
    res = %Transaction{empty() | actions: actions, delta: delta}

    assert {:error, _} = Transaction.verify(res)

    # the delta should be negative let's say we can jam and cue this
    noun = res |> Noun.Nounable.to_noun()

    assert noun |> Noun.Jam.jam() |> Noun.Jam.cue()
    assert Transaction.from_noun(noun) == {:ok, res}

    res
  end

  @spec nullify_intent() :: Transaction.t()
  def nullify_intent() do
    actions =
      MapSet.new([EAction.trivial_true_2_nullifier_action()])

    delta = EAction.trivial_true_2_nullifier_delta()
    %Transaction{empty() | actions: actions, delta: delta}
  end

  @spec swap_from_actions() :: Transaction.t()
  def swap_from_actions() do
    res = Transaction.compose(nullify_intent_eph(), commit_intent())

    assert %{} = res.delta
    assert Transaction.verify(res)

    res
  end

  @spec swap_from_actions_random() :: Transaction.t()
  def swap_from_actions_random() do
    res = Transaction.compose(nullify_intent_eph_rand(), commit_intent())

    assert %{} = res.delta
    assert Transaction.verify(res)

    res
  end

  # This should fail verification on submission if storage is not
  # primed
  @spec swap_from_actions_non_eph_nullifier() :: Transaction.t()
  def swap_from_actions_non_eph_nullifier() do
    res = Transaction.compose(nullify_intent(), commit_intent())

    assert %{} = res.delta
    assert Transaction.verify(res)

    res
  end
end
