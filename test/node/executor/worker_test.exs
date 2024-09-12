defmodule AnomaTest.Node.Executor.Worker do
  use TestHelper.TestMacro, async: true

  alias Anoma.Resource.Delta
  alias Anoma.Node.{Storage, Ordering, Router}
  alias Anoma.Node.Executor.Worker
  alias Anoma.Node.Router.Engine
  import TestHelper.Nock

  setup_all do
    storage = %Storage{
      qualified: AnomaTest.Worker.Qualified,
      order: AnomaTest.Worker.Order
    }

    {:ok, router, _} = Anoma.Node.Router.start()

    {:ok, storage} =
      Anoma.Node.Router.start_engine(router, Storage, storage)

    {:ok, ordering} =
      Anoma.Node.Router.start_engine(router, Ordering, storage: storage)

    snapshot_path = [:my_special_nock_snaphsot | 0]

    env = %Nock{snapshot_path: snapshot_path, ordering: ordering}

    [env: env, router: router]
  end

  test "successful worker", %{env: env, router: router} do
    {:ok, topic} = Router.new_topic(router)
    :ok = Router.call(router, {:subscribe_topic, topic, :local})

    key = 555
    id_1 = System.unique_integer([:positive])
    id_2 = System.unique_integer([:positive])

    storage = Engine.get_state(env.ordering).storage
    increment = increment_counter_val(key)

    Storage.ensure_new(storage)
    Ordering.reset(env.ordering)

    {:ok, spawn_1} =
      Router.start_engine(
        router,
        Worker,
        {id_1, {:kv, increment}, env, topic, nil}
      )

    {:ok, spawn_2} =
      Router.start_engine(
        router,
        Worker,
        {id_2, {:kv, increment}, env, topic, nil}
      )

    # simulate sending in 2 different orders
    ord_1 = Engine.get_state(env.ordering).next_order

    Ordering.new_order(env.ordering, [
      Anoma.Transaction.new_with_order(ord_1, id_1, spawn_1)
    ])

    ord_2 = Engine.get_state(env.ordering).next_order

    Ordering.new_order(env.ordering, [
      Anoma.Transaction.new_with_order(ord_2, id_2, spawn_2)
    ])

    # Setup default value for storage
    Storage.put(storage, key, 0)
    # Now set the snapshot up that scry expects
    Storage.put_snapshot(storage, hd(env.snapshot_path))
    # tell the first spawn it can write
    Router.send_raw(spawn_1, {:write_ready, 1})
    TestHelper.Worker.wait_for_worker(spawn_1, :ok)
    assert {:ok, 1} == Storage.get(storage, key)

    Router.send_raw(spawn_2, {:write_ready, 2})
    TestHelper.Worker.wait_for_worker(spawn_2, :ok)
    assert {:ok, 2} == Storage.get(storage, key)

    :ok = Router.call(router, {:unsubscribe_topic, topic, :local})
  end

  test "failed worker", %{env: env, router: router} do
    {:ok, topic} = Router.new_topic(router)
    :ok = Router.call(router, {:subscribe_topic, topic, :local})

    key = 555
    id = System.unique_integer([:positive])

    storage = Engine.get_state(env.ordering).storage
    increment = increment_counter_val(key)

    Storage.ensure_new(storage)
    Ordering.reset(env.ordering)

    {:ok, spawn} =
      Router.start_engine(
        router,
        Worker,
        {id, {:kv, increment}, env, topic, nil}
      )

    Ordering.new_order(env.ordering, [
      Anoma.Transaction.new_with_order(1, id, spawn)
    ])

    # do not setup storage, just snapshot with our key
    Storage.put_snapshot(storage, hd(env.snapshot_path))
    # check we are alive even though we failed
    assert Process.alive?(Router.Addr.pid(spawn))

    Router.send_raw(spawn, {:write_ready, 1})
    TestHelper.Worker.wait_for_worker(spawn, :error)
    # check that we snapshotted
    assert {:atomic, [{_, _, 2}]} =
             Storage.read_order_tx(storage, hd(env.snapshot_path))

    :ok = Router.call(router, {:unsubscribe_topic, topic, :local})
  end

  test "worker read value", %{env: env, router: router} do
    {:ok, topic} = Router.new_topic(router)
    :ok = Router.call(router, {:subscribe_topic, topic, :local})

    key = 555

    order_id = System.unique_integer([:positive])

    storage = Engine.get_state(env.ordering).storage
    plus_one = counter_val_plus_one(key)

    Storage.ensure_new(storage)
    Ordering.reset(env.ordering)

    # Setup default value for storage
    Storage.put(storage, key, 999)
    # Now set the snapshot up that scry expects
    Storage.put_snapshot(storage, hd(env.snapshot_path))

    {:ok, worker} =
      Router.start_engine(
        router,
        Worker,
        {order_id, {:ro, plus_one}, env, topic, Router.self_addr()}
      )

    idx = Engine.get_state(env.ordering).next_order

    Ordering.new_order(env.ordering, [
      Anoma.Transaction.new_with_order(idx, order_id, worker)
    ])

    # receive value from reply-to address
    TestHelper.Worker.wait_for_read_value(1000)
    # receive value from the worker topic subscription
    TestHelper.Worker.wait_for_read_value(1000)
    TestHelper.Worker.wait_for_worker(worker, :ok)
  end

  test "failed worker waits for a snapshot before write", %{
    env: env,
    router: router
  } do
    {:ok, topic} = Router.new_topic(router)
    :ok = Router.call(router, {:subscribe_topic, topic, :local})

    id = System.unique_integer([:positive])

    storage = Engine.get_state(env.ordering).storage
    bogus = [0 | 1]

    Storage.ensure_new(storage)
    Ordering.reset(env.ordering)

    {:ok, spawn} =
      Router.start_engine(router, Worker, {id, {:kv, bogus}, env, topic, nil})

    Ordering.new_order(env.ordering, [
      Anoma.Transaction.new_with_order(1, id, spawn)
    ])

    # we say that it can write, however we should still be alive, due
    # to the storage snapshot not being ready for it
    Router.send_raw(spawn, {:write_ready, 1})
    assert Process.alive?(Router.Addr.pid(spawn))
    # do not setup storage, just snapshot with our key
    Storage.put_snapshot(storage, hd(env.snapshot_path))
    # the storage is there we should be done now
    TestHelper.Worker.wait_for_worker(spawn, :error)
    :ok = Router.call(router, {:unsubscribe_topic, topic, :local})
  end

  test "worker evaluates resource transaction, no duble spending", %{
    env: env,
    router: router
  } do
    import Anoma.Resource
    alias Anoma.Resource.ProofRecord
    alias Anoma.Resource.Transaction

    {:ok, topic} = Router.new_topic(router)
    :ok = Router.call(router, {:subscribe_topic, topic, :local})

    id = System.unique_integer([:positive])
    id_2 = System.unique_integer([:positive])

    storage = Engine.get_state(env.ordering).storage

    Storage.ensure_new(storage)
    Ordering.reset(env.ordering)

    keypair = Anoma.Crypto.Sign.new_keypair()

    in_resource = %{
      new_with_npk(keypair.public)
      | label: "space bucks",
        quantity: 10
    }

    nf_in = nullifier(in_resource, keypair.secret)
    pf_in = ProofRecord.prove(in_resource)

    out_resource = %{
      new_with_npk(keypair.public)
      | label: "space bucks",
        quantity: 10
    }

    cm_out = commitment(out_resource)
    pf_out = ProofRecord.prove(out_resource)

    rm_tx = %Transaction{
      commitments: [cm_out],
      nullifiers: [nf_in],
      proofs: [pf_in, pf_out],
      delta: Delta.empty()
    }

    rm_tx_noun = Noun.Nounable.to_noun(rm_tx)
    rm_executor_tx = [[1 | rm_tx_noun], 0 | 0]

    {:ok, spawn} =
      Router.start_engine(
        router,
        Worker,
        {id, {:rm, rm_executor_tx}, env, topic, nil}
      )

    Ordering.new_order(env.ordering, [
      Anoma.Transaction.new_with_order(0, id, spawn)
    ])

    {:ok, spawn_1} =
      Router.start_engine(
        router,
        Worker,
        {id_2, {:rm, rm_executor_tx}, env, topic, nil}
      )

    Ordering.new_order(env.ordering, [
      Anoma.Transaction.new_with_order(1, id_2, spawn_1)
    ])

    Router.send_raw(spawn, {:write_ready, 0})
    TestHelper.Worker.wait_for_worker(spawn, :ok)

    Router.send_raw(spawn_1, {:write_ready, 1})
    TestHelper.Worker.wait_for_worker(spawn_1, :error)

    :ok = Router.call(router, {:unsubscribe_topic, topic, :local})
  end

  test "worker verifies cairo proofs", %{env: env, router: router} do
    alias Anoma.ShieldedResource.ShieldedTransaction
    alias Anoma.ShieldedResource.PartialTransaction
    alias Anoma.ShieldedResource.ProofRecord
    alias Anoma.ShieldedResource
    alias Anoma.Constants
    alias Anoma.ShieldedResource.ComplianceInput
    alias Anoma.ShieldedResource.ResourceTree

    id = System.unique_integer([:positive])

    storage = Engine.get_state(env.ordering).storage

    {:ok, topic} = Router.new_topic(router)
    :ok = Router.call(router, {:subscribe_topic, topic, :local})
    Storage.ensure_new(storage)
    Ordering.reset(env.ordering)

    # create an input resource
    input_nf_key = :binary.copy(<<0>>, 31) <> <<1>>

    input_resource = %ShieldedResource{
      # There is a trivial resource logic, params/trivial_resource_logic.juvix
      logic: Constants.cairo_trivial_resource_logic_hash(),
      label: <<0::256>>,
      quantity: :binary.copy(<<0>>, 31) <> <<5>>,
      data: <<0::256>>,
      eph: false,
      nonce: <<0::256>>,
      npk: ShieldedResource.get_npk(input_nf_key),
      rseed: <<0::256>>
    }

    eph_root = Cairo.random_felt() |> :binary.list_to_bin()

    # create an output resource
    input_nf = ShieldedResource.nullifier(input_resource)
    output_resource = ShieldedResource.set_nonce(input_resource, input_nf)
    output_cm = ShieldedResource.commitment(output_resource)

    # rcv is used to generate the binding signature(delta proof)
    rcv = :binary.copy(<<0>>, 31) <> <<3>>

    # Mock cm tree history
    cm_tree =
      CommitmentTree.new(
        CommitmentTree.Spec.cairo_poseidon_cm_tree_spec(),
        Anoma.Node.Router.Engine.get_state(storage).cairo_rm_commitments
      )

    input_resource_cm = ShieldedResource.commitment(input_resource)
    # Insert the input resouce to the tree
    {ct, anchor} = CommitmentTree.add(cm_tree, [input_resource_cm])
    # Get the merkle proof of the input resource
    merkle_proof = CommitmentTree.prove(ct, 0)
    # Insert the cuurent root to the storage
    Storage.put(storage, ["rm", "commitment_root", anchor], true)

    # Generate the compliance inputs
    compliance_inputs =
      %ComplianceInput{
        input_resource: input_resource,
        merkel_proof: merkle_proof,
        output_resource: output_resource,
        input_nf_key: input_nf_key,
        eph_root: eph_root,
        rcv: rcv
      }
      |> ComplianceInput.to_json_string()

    {:ok, compliance_proof} =
      ProofRecord.generate_compliance_proof(compliance_inputs)

    # Build resource logic proofs
    {:ok, resource_logic_circuit} =
      File.read("./params/trivial_resource_logic.json")

    # Build resource merkle tree
    rt =
      ResourceTree.construct(
        CommitmentTree.Spec.cairo_poseidon_resource_tree_spec(),
        [input_nf, output_cm]
      )

    _input_resource_path = ResourceTree.prove(rt, input_nf)
    # IO.inspect(_input_resource_path)

    # Application developers should be responsible to define and fill up the input_resource_logic_inputs json
    input_resource_logic_inputs = """
    {
      "self_resource": {
          "logic" : "0x1e3a7d104e0f5f7cec0e9a750ad7df223df65163bed9451fad655ebae92964a",
          "label" : "0x0",
          "quantity" : "0x5",
          "data" : "0x0",
          "eph" : false,
          "nonce" : "0x0",
          "npk" : "0x7752582c54a42fe0fa35c40f07293bb7d8efe90e21d8d2c06a7db52d7d9b7a1",
          "rseed" : "0x0"
      },
      "resource_nf_key": "0x1",
      "merkle_path": [{"fst": "0x1e15dfe74215466d6038437c7e0c27f9c71353b90f51f586898f515d3d3f7d1", "snd": false}, {"fst": "0x0", "snd": false}, {"fst": "0x0", "snd": false}, {"fst": "0x0", "snd": false}]
    }
    """

    {:ok, input_resource_logic_proof} =
      ProofRecord.generate_cairo_proof(
        resource_logic_circuit,
        input_resource_logic_inputs
      )

    _output_resource_path_ = ResourceTree.prove(rt, output_cm)
    # IO.inspect(_output_resource_path_)

    output_resource_logic_inputs = """
    {
      "self_resource": {
          "logic" : "0x1e3a7d104e0f5f7cec0e9a750ad7df223df65163bed9451fad655ebae92964a",
          "label" : "0x0",
          "quantity" : "0x5",
          "data" : "0x0",
          "eph" : false,
          "nonce" : "0x512202c0970b0ec15a9eeee086927b8021d03c4c2e76cbcd41ef5f4f59a3847",
          "npk" : "0x7752582c54a42fe0fa35c40f07293bb7d8efe90e21d8d2c06a7db52d7d9b7a1",
          "rseed" : "0x0"
      },
      "resource_nf_key": "0x1",
      "merkle_path": [{"fst": "0x512202c0970b0ec15a9eeee086927b8021d03c4c2e76cbcd41ef5f4f59a3847", "snd": true}, {"fst": "0x0", "snd": false}, {"fst": "0x0", "snd": false}, {"fst": "0x0", "snd": false}]
    }
    """

    {:ok, output_resource_logic_proof} =
      ProofRecord.generate_cairo_proof(
        resource_logic_circuit,
        output_resource_logic_inputs
      )

    ptx = %PartialTransaction{
      logic_proofs: [input_resource_logic_proof, output_resource_logic_proof],
      compliance_proofs: [compliance_proof]
    }

    # Generate the binding signature
    # priv_keys are from rcvs in compliance_inputs
    priv_keys = :binary.copy(<<0>>, 31) <> <<3>>

    rm_tx =
      %ShieldedTransaction{
        partial_transactions: [ptx],
        delta: priv_keys
      }
      |> ShieldedTransaction.finalize()

    rm_tx_noun = Noun.Nounable.to_noun(rm_tx)
    rm_executor_tx = [[1 | rm_tx_noun], 0 | 0]

    {:ok, spawn} =
      Router.start_engine(
        router,
        Worker,
        {id, {:cairo, rm_executor_tx}, env, topic, nil}
      )

    Ordering.new_order(env.ordering, [
      Anoma.Transaction.new_with_order(0, id, spawn)
    ])

    Router.send_raw(spawn, {:write_ready, 0})
    TestHelper.Worker.wait_for_worker(spawn, :ok)
    :ok = Router.call(router, {:unsubscribe_topic, topic, :local})
  end
end
