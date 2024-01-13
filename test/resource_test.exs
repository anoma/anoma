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

  test "logic that rejects nonzero delta" do
    keypair = Sign.new_keypair()

    # this logic just tests that the tx delta equals the zero delta
    resource = %{
      new_with_npk(keypair.public)
      | label: "space bucks",
        quantity: 10,
        logic: Noun.Format.parse_always("[[5 [1 0] [0 222]] 0 0]")
    }

    # valid tx delta (tx burns 10 space bucks).
    tx_delta = %{kind(resource) => -10}

    nf_r = nullifier(resource, keypair.secret)
    pf_r = ProofRecord.prove(resource)

    transaction = %Transaction{
      nullifiers: [nf_r],
      proofs: [pf_r],
      delta: tx_delta
    }

    # todo: break out the verifier so we can assert specifically
    # that there is a failure in a resource logic
    refute Transaction.verify(transaction)

    # the same resource should allow itself to be spent if balanced
    balancing_resource = %{
      new_with_npk(keypair.public)
      | label: "space bucks",
        quantity: 10,
        logic: Noun.Format.parse_always("[[5 [1 0] [0 222]] 0 0]")
    }

    cm_br = commitment(balancing_resource)
    pf_br = ProofRecord.prove(balancing_resource)

    balanced_transaction = %Transaction{
      nullifiers: [nf_r],
      commitments: [cm_br],
      proofs: [pf_r, pf_br],
      delta: %{}
    }

    assert Transaction.verify(balanced_transaction)
  end

  test "counter logic" do
    counter_logic = [
      Noun.Format.parse_always("""
      [ 6
        [5 [1 1] 8 [9 1.406 0 127] 9 2 10 [6 0 58] 0 2]
        [ 6
          [5 [1 1] 8 [9 1.406 0 127] 9 2 10 [6 0 118] 0 2]
          [ 6
            [5 [1 1] 8 [9 1.406 0 127] 9 2 10 [6 0 478] 0 2]
            [6 [5 [1 0] 0 222] [0 0] 6 [0 1.778] [1 0] 1 1]
            1
            1
          ]
          1
          1
        ]
        1
        1
      ]
      """),
      0 | Nock.logics_core()
    ]

    keypair = Sign.new_keypair()

    zeroed_counter = %{
      new_with_npk(keypair.public)
      | label: "counter",
        quantity: 0,
        logic: counter_logic
    }

    incremented_counter = %{
      new_with_npk(keypair.public)
      | label: "counter",
        quantity: 1,
        logic: counter_logic
    }

    nf_0 = nullifier(zeroed_counter, keypair.secret)
    pf_0 = ProofRecord.prove(zeroed_counter)

    cm_1 = commitment(incremented_counter)
    pf_1 = ProofRecord.prove(incremented_counter)

    tx = %Transaction{
      commitments: [cm_1],
      nullifiers: [nf_0],
      proofs: [pf_0, pf_1],
      delta: %{kind(zeroed_counter) => 1}
    }

    assert Transaction.verify(tx)
  end
end
