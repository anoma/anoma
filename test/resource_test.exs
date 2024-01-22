defmodule AnomaTest.Resource do
  use ExUnit.Case, async: true
  doctest Anoma.Resource

  import Anoma.Resource
  alias Anoma.Resource.ProofRecord
  alias Anoma.Resource.Transaction
  alias Anoma.Sign

  test "commitments and nullifiers" do
    keypair_a = Sign.new_keypair()
    keypair_b = Sign.new_keypair()

    a_r1 = new_with_npk(keypair_a.public)
    a_r2 = new_with_npk(keypair_a.public)
    b_r0 = new_with_npk(keypair_b.public)

    # just in case
    assert a_r1 != a_r2

    c_a_r1 = commitment(a_r1)
    c_a_r2 = commitment(a_r2)
    c_b_r0 = commitment(b_r0)

    n_a_r1 = nullifier(a_r1, keypair_a.secret)
    n_a_r2 = nullifier(a_r2, keypair_a.secret)
    n_b_r0 = nullifier(b_r0, keypair_b.secret)

    assert c_a_r1 |> commits_to(a_r1)
    refute c_a_r1 |> commits_to(a_r2)
    refute c_a_r1 |> commits_to(b_r0)

    refute c_a_r2 |> commits_to(a_r1)
    assert c_a_r2 |> commits_to(a_r2)
    refute c_a_r2 |> commits_to(b_r0)

    refute c_b_r0 |> commits_to(a_r1)
    refute c_b_r0 |> commits_to(a_r2)
    assert c_b_r0 |> commits_to(b_r0)

    assert n_a_r1 |> nullifies(a_r1)
    refute n_a_r1 |> nullifies(a_r2)
    refute n_a_r1 |> nullifies(b_r0)

    refute n_a_r2 |> nullifies(a_r1)
    assert n_a_r2 |> nullifies(a_r2)
    refute n_a_r2 |> nullifies(b_r0)

    refute n_b_r0 |> nullifies(a_r1)
    refute n_b_r0 |> nullifies(a_r2)
    assert n_b_r0 |> nullifies(b_r0)
  end

  test "nullify with wrong key" do
    keypair_a = Sign.new_keypair()
    keypair_b = Sign.new_keypair()

    a_resource = new_with_npk(keypair_a.public)
    wrong_nullifier = nullifier(a_resource, keypair_b.secret)

    refute wrong_nullifier |> nullifies(a_resource)
  end

  test "resource transaction" do
    keypair = Sign.new_keypair()

    old_resource = %{
      new_with_npk(keypair.public)
      | label: "space bucks",
        quantity: 10
    }

    unbalanced_resource = %{
      new_with_npk(keypair.public)
      | label: "space bucks",
        quantity: 5
    }

    balanced_resource = %{
      new_with_npk(keypair.public)
      | label: "space bucks",
        quantity: 10
    }

    # they contain random nonces, they shouldn't be identical!
    assert old_resource != balanced_resource

    nf_old = nullifier(old_resource, keypair.secret)
    pf_old = ProofRecord.prove(old_resource)

    cm_unb = commitment(unbalanced_resource)
    pf_unb = ProofRecord.prove(unbalanced_resource)

    cm_bal = commitment(balanced_resource)
    pf_bal = ProofRecord.prove(balanced_resource)

    # the zero delta = the empty map.
    zero_delta = %{}

    bad_tx = %Transaction{
      commitments: [cm_unb],
      nullifiers: [nf_old],
      proofs: [pf_old, pf_unb],
      delta: zero_delta
    }

    refute Transaction.verify(bad_tx)

    good_tx = %Transaction{
      commitments: [cm_bal],
      nullifiers: [nf_old],
      proofs: [pf_old, pf_bal],
      delta: zero_delta
    }

    assert Transaction.verify(good_tx)
  end
end
