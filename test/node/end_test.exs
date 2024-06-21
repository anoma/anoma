defmodule AnomaTest.Node.End do
  use ExUnit.Case, async: true

  alias Anoma.Node.Storage
  alias Anoma.Node.Ordering
  alias Anoma.Node.{Router, Mempool, Solver, IntentPool}
  alias Anoma.Crypto.Sign
  import alias Anoma.Resource
  alias Anoma.Resource.Transaction
  alias Anoma.Resource.ProofRecord
  import TestHelper.Nock

  setup_all do
    storage = %Storage{
      qualified: AnomaTest.End.Qualified,
      order: AnomaTest.End.Order
    }

    name = :end
    snapshot_path = [:my_special_nock_snaphsot | 0]

    {:ok, nodes} =
      Anoma.Node.start_link_or_find_instance(
        name: name,
        use_rocks: true,
        testing: true,
        settings:
          {:new_storage,
           [
             snapshot_path: snapshot_path,
             storage_data: storage,
             block_storage: :end_blocks,
             ping_time: :no_timer
           ]
           |> Anoma.Node.start_min()}
      )

    node = Anoma.Node.state(nodes)

    [node: node]
  end

  test "e2e test", %{node: node} do
    router = node.router
    mempool = node.mempool

    {:ok, intents} = Router.new_topic(router)
    {:ok, solutions} = Router.new_topic(router)
    {:ok, ip} = Router.start_engine(router, IntentPool, {intents, nil})

    {:ok, s} =
      Router.start_engine(
        router,
        Solver,
        {router, nil, ip, intents, solutions}
      )

    key = 555
    storage = Ordering.get_storage(node.ordering)
    zero = zero_counter(key)
    increment = increment_counter_val(key)

    Mempool.hard_reset(mempool)

    pid_zero = Mempool.tx(mempool, {:kv, zero}).pid

    :ok =
      Router.call(
        router,
        {:subscribe_topic, node.executor_topic.id, :local}
      )

    assert {:ok, 1} = Mempool.execute(mempool)
    assert_receive({:"$gen_cast", {_, _, {:process_done, ^pid_zero}}}, 5000)

    keya = Sign.new_keypair()
    keyb = Sign.new_keypair()
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

    :ok = Router.call(router, {:subscribe_topic, solutions, :local})

    IntentPool.new_intent(ip, tx1)
    IntentPool.new_intent(ip, tx2)

    :ok = Router.call(router, {:subscribe_topic, node.mempool_topic, :local})

    assert_receive {:"$gen_cast", {_, _, {:solutions, _}}}

    Solver.mempool_send(s, mempool)

    pid_one = Mempool.tx(node.mempool, {:kv, increment}).pid

    assert_receive {:"$gen_cast", {_, _, {:submitted, _}}}

    assert {:ok, 2} = Mempool.execute(mempool)

    assert_receive {:"$gen_cast", {_, _, {:process_done, ^pid_one}}}

    assert {:ok, 1} = Storage.get(storage, key)

    :ok =
      Router.call(
        node.router,
        {:unsubscribe_topic, node.executor_topic.id, :local}
      )

    :ok =
      Router.call(
        node.router,
        {:unsubscribe_topic, node.mempool_topic, :local}
      )

    :ok =
      Router.call(
        node.router,
        {:unsubscribe_topic, solutions, :local}
      )
  end
end
