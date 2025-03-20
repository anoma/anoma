defmodule Examples.ETransparent.ELogicProof do
  alias Anoma.RM.Transparent.ProvingSystem.RLPS
  alias Anoma.RM.Transparent.Resource
  alias Examples.ETransparent.EResource

  use TestHelper.TestMacro

  @spec trivial_true_commitment() :: RLPS.Instance.t()
  def trivial_true_commitment() do
    cm = EResource.trivial_true_commitment()

    res = %RLPS.Instance{
      tag: EResource.trivial_true_commitment(),
      created: [EResource.trivial_true_commitment()]
    }

    {:ok, result} = RLPS.match_resource(cm, false)

    assert RLPS.verify(
             result.logicref |> Noun.atom_integer_to_binary(),
             res,
             <<>>
           )

    # This should not fail
    assert {:ok, _} =
             res
             |> Noun.Nounable.to_noun()
             |> Noun.Jam.jam()
             |> Noun.Jam.cue!()
             |> RLPS.Instance.from_noun()

    res
  end

  @spec trivial_true_2_nullifier() :: RLPS.Instance.t()
  def trivial_true_2_nullifier() do
    nlf =
      Resource.nullifier_hash(<<0::256>>, EResource.trivial_true_resource_2())

    res = %RLPS.Instance{
      tag: nlf,
      flag: true,
      consumed: [EResource.trivial_true_nullifier_2()]
    }

    {:ok, result} = RLPS.match_resource(nlf, true)

    assert RLPS.verify(
             result.logicref |> Noun.atom_integer_to_binary(),
             res,
             <<>>
           )

    res
  end

  @spec trivial_true_eph_nullifier() :: RLPS.Instance.t()
  def trivial_true_eph_nullifier() do
    nlf =
      Resource.nullifier_hash(
        <<0::256>>,
        EResource.trivial_true_resource_eph()
      )

    res = %RLPS.Instance{
      tag: nlf,
      flag: true,
      consumed: [EResource.trivial_true_nullifier_eph()]
    }

    {:ok, result} = RLPS.match_resource(nlf, true)

    assert RLPS.verify(
             result.logicref |> Noun.atom_integer_to_binary(),
             res,
             <<>>
           )

    res
  end

  @spec trivial_true_swap_proof_commitment() :: RLPS.Instance.t()
  def trivial_true_swap_proof_commitment() do
    nullifier_info = trivial_true_2_nullifier()

    res = %RLPS.Instance{
      trivial_true_commitment()
      | consumed: nullifier_info.consumed
    }

    {:ok, result} = RLPS.match_resource(res.tag, false)

    assert RLPS.verify(
             result.logicref |> Noun.atom_integer_to_binary(),
             res,
             <<>>
           )

    res
  end

  @spec trivial_true_swap_proof_nullifier() :: RLPS.Instance.t()
  def trivial_true_swap_proof_nullifier() do
    commitment_info = trivial_true_commitment()

    res = %RLPS.Instance{
      trivial_true_2_nullifier()
      | created: commitment_info.created
    }

    {:ok, result} = RLPS.match_resource(res.tag, true)

    assert RLPS.verify(
             result.logicref |> Noun.atom_integer_to_binary(),
             res,
             <<>>
           )

    res
  end

  @spec trivial_false_proof() :: RLPS.Instance.t()
  def trivial_false_proof() do
    # We don't need much, since the verify just runs the function
    cm = EResource.trivial_false_resource() |> Resource.commitment_hash()

    res = %RLPS.Instance{
      tag: cm,
      flag: false
    }

    {:ok, result} = RLPS.match_resource(res.tag, false)

    refute RLPS.verify(
             result.logicref |> Noun.atom_integer_to_binary(),
             res,
             <<>>
           )

    res
  end
end
