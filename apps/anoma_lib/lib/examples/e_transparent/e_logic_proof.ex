defmodule Examples.ETransparent.ELogicProof do
  alias Anoma.TransparentResource.LogicProof
  alias Examples.ETransparent.EResource

  use TestHelper.TestMacro

  def trivial_true_swap_proof_commitment() do
    res = %LogicProof{
      resource: EResource.trivial_true_resource(),
      self_tag: {:committed, EResource.trivial_true_commitment()},
      commitments: MapSet.new([EResource.trivial_true_commitment()]),
      nullifiers: MapSet.new([EResource.trivial_true_nullifier_2()]),
      committed_plaintexts: MapSet.new([EResource.trivial_true_resource()]),
      nullified_plaintexts: MapSet.new([EResource.trivial_true_resource_2()])
    }

    assert LogicProof.verify(res)

    res
  end

  def trivial_true_swap_proof_nullifier() do
    res = %LogicProof{
      resource: EResource.trivial_true_resource_2(),
      self_tag: {:nullified, EResource.trivial_true_nullifier_2()},
      commitments: MapSet.new([EResource.trivial_true_commitment()]),
      nullifiers: MapSet.new([EResource.trivial_true_nullifier_2()]),
      committed_plaintexts: MapSet.new([EResource.trivial_true_resource()]),
      nullified_plaintexts: MapSet.new([EResource.trivial_true_resource_2()])
    }

    assert LogicProof.verify(res)

    res
  end

  def trivial_false_proof() do
    # We don't need much, since the verify just runs the function
    res = %LogicProof{
      resource: EResource.trivial_false_resource(),
      self_tag: {:committed, EResource.trivial_false_commitment()}
    }

    refute LogicProof.verify(res)

    res
  end
end
