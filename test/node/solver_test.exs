defmodule AnomaTest.Node.Solver do
  use ExUnit.Case, async: true

  test "x for y" do
    import alias Anoma.Resource
    alias Anoma.Resource.Transaction
    alias Anoma.Resource.ProofRecord
    alias Anoma.Node.Solver

    keya = Anoma.Crypto.Sign.new_keypair()
    keyb = Anoma.Crypto.Sign.new_keypair()
    rxa = %{new_with_npk(keya.public) | label: "x", quantity: 1}
    rxb = %{new_with_npk(keyb.public) | label: "x", quantity: 1}
    rya = %{new_with_npk(keya.public) | label: "y", quantity: 1}
    ryb = %{new_with_npk(keyb.public) | label: "y", quantity: 1}

    tx1 = %Transaction{
      proofs: [ProofRecord.prove(rxa), ProofRecord.prove(rya)],
      commitments: [commitment(rxa)],
      nullifiers: [Resource.nullifier(rya, keya.secret)]
    }

    tx2 = %Transaction{
      proofs: [ProofRecord.prove(rxb), ProofRecord.prove(ryb)],
      commitments: [commitment(ryb)],
      nullifiers: [Resource.nullifier(rxb, keyb.secret)]
    }

    Solver.start_link(:solver_test)
    Solver.Communicator.subscribe(:solver_test_com, self(), false)

    Solver.Communicator.add_intent(:solver_test_com, tx1)
    Solver.Communicator.add_intent(:solver_test_com, tx2)

    receive do
      {:"$gen_cast", {:solutions, [solution]}} ->
        assert Enum.all?([commitment(rxa), commitment(ryb)], fn x ->
                 x in solution.commitments
               end)

        assert Enum.all?(
                 [
                   ProofRecord.prove(rxa),
                   ProofRecord.prove(rya),
                   ProofRecord.prove(rxb),
                   ProofRecord.prove(ryb)
                 ],
                 fn x -> x in solution.proofs end
               )
    end
  end
end
