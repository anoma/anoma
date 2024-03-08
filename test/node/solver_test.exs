defmodule AnomaTest.Node.Solver do
  use ExUnit.Case, async: true

  test "x for y" do
    import alias Anoma.Resource
    alias Anoma.Resource.Transaction
    alias Anoma.Resource.ProofRecord
    alias Anoma.Node.Solver
    alias Anoma.Node.IntentPool
    alias Anoma.Node.Router

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

    {:ok, router} = Router.start()
    {:ok, intents} = Router.new_topic(router)
    {:ok, solutions} = Router.new_topic(router)

    {:ok, iip} =
      Router.start_engine(router, IntentPool, {intents, nil})

    {:ok, _sip} =
      Router.start_engine(
        router,
        Solver,
        {router, nil, iip, intents, solutions}
      )

    :ok = Router.call(router, {:subscribe_topic, solutions, :local})

    IntentPool.new_intent(iip, tx1)
    IntentPool.new_intent(iip, tx2)

    receive do
      {:"$gen_cast", {_, {:solutions, [solution]}}} ->
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
