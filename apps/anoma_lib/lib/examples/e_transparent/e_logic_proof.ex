defmodule Examples.ETransparent.ELogicProof do
  alias Anoma.TransparentResource.LogicProof
  alias Examples.ETransparent.EResource

  use TestHelper.TestMacro

  def trivial_true_commitment() do
    res = %LogicProof{
      resource: EResource.trivial_true_resource(),
      self_tag: {:committed, EResource.trivial_true_commitment()},
      commitments: MapSet.new([EResource.trivial_true_commitment()]),
      committed_plaintexts: MapSet.new([EResource.trivial_true_resource()]),
      other_private: 0,
      other_public: 0
    }

    assert LogicProof.verify(res)

    # This should not fail
    assert {:ok, _} =
             res
             |> Noun.Nounable.to_noun()
             |> Nock.Jam.jam()
             |> Nock.Cue.cue()
             |> elem(1)
             |> LogicProof.from_noun()

    res
  end

  def trivial_true_2_nullifier() do
    res = %LogicProof{
      resource: EResource.trivial_true_resource_2(),
      self_tag: {:nullified, EResource.trivial_true_nullifier_2()},
      nullifiers: MapSet.new([EResource.trivial_true_nullifier_2()]),
      nullified_plaintexts: MapSet.new([EResource.trivial_true_resource_2()])
    }

    assert LogicProof.verify(res)

    res
  end

  def trivial_true_eph_nullifier() do
    res = %LogicProof{
      resource: EResource.trivial_true_resource_eph(),
      self_tag: {:nullified, EResource.trivial_true_nullifier_eph()},
      nullifiers: MapSet.new([EResource.trivial_true_nullifier_eph()]),
      nullified_plaintexts:
        MapSet.new([EResource.trivial_true_resource_eph()])
    }

    assert LogicProof.verify(res)

    res
  end

  def trivial_true_swap_proof_commitment() do
    nullifier_info = trivial_true_2_nullifier()

    res = %LogicProof{
      trivial_true_commitment()
      | nullifiers: nullifier_info.nullifiers,
        nullified_plaintexts: nullifier_info.nullified_plaintexts
    }

    assert LogicProof.verify(res)

    res
  end

  def trivial_true_swap_proof_nullifier() do
    commitment_info = trivial_true_commitment()

    res = %LogicProof{
      trivial_true_2_nullifier()
      | commitments: commitment_info.commitments,
        committed_plaintexts: commitment_info.committed_plaintexts
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
