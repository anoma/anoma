defmodule Examples.ETransparent.EAction do
  alias Examples.ETransparent.EResource
  alias Anoma.RM.Transparent.Action
  alias Anoma.RM.Transparent.Resource
  alias Anoma.RM.Transparent.Primitive.CommitmentAccumulator

  use TestHelper.TestMacro

  @spec empty() :: Action.t()
  def empty() do
    res = %Action{}

    assert 2 = Action.delta(res)
    assert Action.verify(res)
    res
  end

  @spec trivial_swap_action() :: Action.t()
  def trivial_swap_action() do
    consumed = EResource.trivial_true_resource_2()
    created = EResource.trivial_true_resource()
    cm = consumed |> Resource.commitment_hash()
    root = MapSet.new([cm]) |> CommitmentAccumulator.value()

    res =
      Action.create(
        [{<<0::256>>, consumed, root}],
        [created],
        %{}
      )

    assert 2 = Action.delta(res)
    assert Action.verify(res)

    res
  end

  @spec trivial_true_commit_action() :: Action.t()
  def trivial_true_commit_action() do
    created = EResource.trivial_true_resource()
    # not balanced
    res = Action.create([], [created], %{})
    # even if unbalanced, the verify goes through
    # the action check does not do that
    assert Action.verify(res)
    res
  end

  @spec trivial_true_commit_delta() :: integer()
  def trivial_true_commit_delta() do
    res = trivial_true_commit_action() |> Action.delta()
    assert 2 != res
    res
  end

  @spec trivial_true_2_nullifier_action() :: Action.t()
  def trivial_true_2_nullifier_action() do
    consumed = EResource.trivial_true_resource_2()
    cm = EResource.trivial_true_resource_2() |> Resource.commitment_hash()
    root = MapSet.new([cm]) |> CommitmentAccumulator.value()

    res =
      Action.create([{<<0::256>>, consumed, root}], [], %{})

    assert Action.verify(res)
    res
  end

  @spec trivial_true_eph_nullifier_action() :: Action.t()
  def trivial_true_eph_nullifier_action() do
    consumed = EResource.trivial_true_resource_eph()
    cm = EResource.trivial_true_resource_eph() |> Resource.commitment_hash()
    root = MapSet.new([cm]) |> CommitmentAccumulator.value()

    res =
      Action.create([{<<0::256>>, consumed, root}], [], %{})

    assert Action.verify(res)

    res
  end
end
