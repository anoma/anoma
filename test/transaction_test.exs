defmodule AnomaTest.Transaction do
  use ExUnit.Case, async: true
  doctest Anoma.Transaction

  import alias Anoma.Transaction
  import Anoma.Resource
  import Anoma.ProofRecord

  test "consumable resource" do
    keypair = Anoma.Sign.new_keypair()

    # the default logic is always-true
    resource_to_consume = %{
      new_with_npk(keypair.public)
      | label: "cool resource",
        quantity: 10
    }

    resource_to_produce = %{
      new_with_npk(keypair.public)
      | label: "cool resource",
        quantity: 10
    }

    # generate the derived values
    n_to_consume = nullifier(resource_to_consume, keypair.secret)
    p_to_consume = prove(resource_to_consume)

    c_to_produce = commitment(resource_to_produce)
    p_to_produce = prove(resource_to_produce)

    # build the transaction
    tx = %Transaction{
      proofs: [p_to_produce, p_to_consume],
      commitments: [c_to_produce],
      nullifiers: [n_to_consume]
    }

    assert verify(tx)
  end

  test "make change" do
    keypair = Anoma.Sign.new_keypair()

    resource_to_consume = %{
      new_with_npk(keypair.public)
      | label: "cool resource",
        quantity: 10
    }

    n_to_consume = nullifier(resource_to_consume, keypair.secret)
    p_to_consume = prove(resource_to_consume)

    resource_to_produce_1 = %{
      new_with_npk(keypair.public)
      | label: "cool resource",
        quantity: 5
    }

    c_to_produce_1 = commitment(resource_to_produce_1)
    p_to_produce_1 = prove(resource_to_produce_1)

    resource_to_produce_2 = %{
      new_with_npk(keypair.public)
      | label: "cool resource",
        quantity: 5
    }

    c_to_produce_2 = commitment(resource_to_produce_2)
    p_to_produce_2 = prove(resource_to_produce_2)

    tx = %Transaction{
      proofs: [p_to_consume, p_to_produce_1, p_to_produce_2],
      commitments: [c_to_produce_1, c_to_produce_2],
      nullifiers: [n_to_consume]
    }

    assert verify(tx)
  end

  test "logic rejects" do
    keypair = Anoma.Sign.new_keypair()

    resource_to_consume = %{
      new_with_npk(keypair.public)
      | label: "cool resource",
        quantity: 10
    }

    n_to_consume = nullifier(resource_to_consume, keypair.secret)
    p_to_consume = prove(resource_to_consume)

    resource_to_produce = %{
      new_with_npk(keypair.public)
      | label: "cool resource",
        quantity: 10,
        logic: Noun.Format.parse_always("[[1 1] 0 0]")
    }

    c_to_produce = commitment(resource_to_produce)
    p_to_produce = prove(resource_to_produce)

    tx = %Transaction{
      proofs: [p_to_consume, p_to_produce],
      commitments: [c_to_produce],
      nullifiers: [n_to_consume]
    }

    refute verify(tx)
  end
end
