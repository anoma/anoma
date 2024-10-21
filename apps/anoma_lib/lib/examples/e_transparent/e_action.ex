defmodule Examples.ETransparent.EAction do
  alias Anoma.TransparentResource.Action
  alias Examples.ETransparent.ELogicProof

  use TestHelper.TestMacro

  def empty() do
    res = %Action{}

    assert Action.verify_correspondence(res)
    assert %{} = Action.delta(res)
    res
  end

  def trivial_action_only_proofs() do
    res = %Action{
      empty()
      | proofs: MapSet.new([ELogicProof.trivial_true_swap_proof_commitment()])
    }

    assert {:error, _} = Action.verify_correspondence(res)

    res
  end

  def trivial_action_proofs_proof_and_commitments() do
    commitments = ELogicProof.trivial_true_swap_proof_commitment().commitments

    res = %Action{
      trivial_action_only_proofs()
      | commitments: commitments
    }

    assert {:error, _} = Action.verify_correspondence(res)

    res
  end
end
