defmodule AnomaTest.Transaction do
  use ExUnit.Case, async: true

  alias Anoma.Resource.{Transaction, ProofRecord}

  test "x for y" do
    Anoma.Node.Solver.start_link(:rpctest_solver)
    Anoma.Node.Intent.start_link(:rpctest_intent)

    Supervisor.start_link(
      [
        {GRPC.Server.Supervisor,
         endpoint: RPC.ValidatorEndpoint, port: 24768, start_server: true}
      ],
      strategy: :one_for_one
    )

    RPC.SolverSupervisor.start_link(
      {:rpctest_solver_com, "localhost", 24769, "localhost", 24768}
    )

    RPC.IntentSupervisor.start_link({:rpctest_intent_com, "localhost", 24767})
    RPC.IntentForwarder.start_link({:rpctest_intent_com, "localhost", 24769})

    {:ok, intent_conn} = GRPC.Stub.connect("localhost:24767")
    keya = Anoma.Crypto.Sign.new_keypair()
    keyb = Anoma.Crypto.Sign.new_keypair()

    rxa = %{
      Anoma.Resource.new_with_npk(keya.public)
      | label: "x",
        quantity: 1
    }

    rxb = %{
      Anoma.Resource.new_with_npk(keyb.public)
      | label: "x",
        quantity: 1
    }

    rya = %{
      Anoma.Resource.new_with_npk(keya.public)
      | label: "y",
        quantity: 1
    }

    ryb = %{
      Anoma.Resource.new_with_npk(keyb.public)
      | label: "y",
        quantity: 1
    }

    tx1 = %Transaction{
      proofs: [ProofRecord.prove(rxa), ProofRecord.prove(rya)],
      commitments: [Anoma.Resource.commitment(rxa)],
      nullifiers: [Anoma.Resource.nullifier(rya, keya.secret)]
    }

    tx2 = %Transaction{
      proofs: [ProofRecord.prove(rxb), ProofRecord.prove(ryb)],
      commitments: [Anoma.Resource.commitment(ryb)],
      nullifiers: [Anoma.Resource.nullifier(rxb, keyb.secret)]
    }

    AnomaInterface.IntentPool.Stub.add_intent(
      intent_conn,
      RPC.Convert.serialise_transaction(tx1)
    )

    AnomaInterface.IntentPool.Stub.add_intent(
      intent_conn,
      RPC.Convert.serialise_transaction(tx2)
    )
  end
end
