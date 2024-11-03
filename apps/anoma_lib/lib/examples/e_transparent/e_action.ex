defmodule Examples.ETransparent.EAction do
  alias Anoma.TransparentResource.Delta
  alias Anoma.TransparentResource.Action
  alias Examples.ETransparent.ELogicProof

  use TestHelper.TestMacro

  @spec empty() :: Action.t()
  def empty() do
    res = %Action{}

    assert Action.verify_correspondence(res)
    assert %{} = Action.delta(res)
    res
  end

  @spec trivial_action_only_proofs() :: Action.t()
  def trivial_action_only_proofs() do
    res = %Action{
      empty()
      | proofs: MapSet.new([ELogicProof.trivial_true_swap_proof_commitment()])
    }

    assert {:error, _} = Action.verify_correspondence(res)

    res
  end

  @spec trivial_action_proofs_proof_and_commitments() :: Action.t()
  def trivial_action_proofs_proof_and_commitments() do
    commitments = ELogicProof.trivial_true_swap_proof_commitment().commitments

    res = %Action{
      trivial_action_only_proofs()
      | commitments: commitments
    }

    assert {:error, _} = Action.verify_correspondence(res)

    res
  end

  @spec trivial_action_proofs_missing_nullifier_proof() :: Action.t()
  def trivial_action_proofs_missing_nullifier_proof() do
    nullifiers = ELogicProof.trivial_true_swap_proof_commitment().nullifiers

    res = %Action{
      trivial_action_proofs_proof_and_commitments()
      | nullifiers: nullifiers
    }

    assert {:error, _} = Action.verify_correspondence(res)

    res
  end

  @spec trivial_swap_action() :: Action.t()
  def trivial_swap_action() do
    action = trivial_action_proofs_missing_nullifier_proof()

    res = %Action{
      trivial_action_proofs_missing_nullifier_proof()
      | proofs:
          MapSet.put(
            action.proofs,
            ELogicProof.trivial_true_swap_proof_nullifier()
          )
    }

    assert Action.verify_correspondence(res)
    assert %{} = Action.delta(res)

    res
  end

  @spec trivial_true_commit_action() :: Action.t()
  def trivial_true_commit_action() do
    logic_proof = ELogicProof.trivial_true_commitment()

    res = %Action{
      empty()
      | proofs: MapSet.new([logic_proof]),
        commitments: logic_proof.commitments
    }

    assert Action.verify_correspondence(res)

    res
  end

  @spec trivial_true_commit_delta() :: Delta.t()
  def trivial_true_commit_delta() do
    trivial_true_commit_action() |> Action.delta()
  end

  @spec trivial_true_2_nullifier_action() :: Action.t()
  def trivial_true_2_nullifier_action() do
    logic_proof = ELogicProof.trivial_true_2_nullifier()

    res = %Action{
      empty()
      | proofs: MapSet.new([logic_proof]),
        nullifiers: logic_proof.nullifiers
    }

    assert Action.verify_correspondence(res)

    res
  end

  @spec trivial_true_eph_nullifier_action() :: Action.t()
  def trivial_true_eph_nullifier_action() do
    logic_proof = ELogicProof.trivial_true_eph_nullifier()

    res = %Action{
      empty()
      | proofs: MapSet.new([logic_proof]),
        nullifiers: logic_proof.nullifiers
    }

    assert Action.verify_correspondence(res)

    res
  end

  @spec trivial_true_2_nullifier_delta() :: Delta.t()
  def trivial_true_2_nullifier_delta() do
    res = trivial_true_2_nullifier_action() |> Action.delta()

    assert res == Delta.negate(trivial_true_commit_delta())

    res
  end
end
