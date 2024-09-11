defmodule Examples.ENode.EShieldedExecution do
  require ExUnit.Assertions
  import ExUnit.Assertions

  alias Anoma.Node.{Storage, Router, Mempool}
  alias Anoma.Node.Executor.Worker
  alias Examples.ENode.EStorage
  alias Examples.{ECommitmentTree,EShieldedTransaction}
  alias TestHelper
  alias Anoma.Node.{Storage, Ordering, Router}
  alias Anoma.Node.Executor.Worker

  @spec execute_valid_transaction() ::  Node.t()
  def execute_valid_transaction() do

    anode = EStorage.august_node("invalid")

    snapshot_path = [:my_special_nock_snaphsot | 0]

    env = %Nock{snapshot_path: snapshot_path, ordering: anode.ordering}

    id = System.unique_integer([:positive])

    {:ok, topic} = Router.new_topic(anode.router)
    :ok = Router.call(anode.router, {:subscribe_topic, topic, :local})
    Storage.ensure_new(anode.storage)
    Ordering.reset(anode.ordering)

    {_ct, _merkle_proof, anchor} = ECommitmentTree.a_merkle_proof()

    # Insert the current root to the storage
    Storage.put(anode.storage, ["rm", "commitment_root", anchor], true)

    shielded_tx = EShieldedTransaction.a_shielded_transaction()

    rm_tx_noun = Noun.Nounable.to_noun(shielded_tx)
    executor_tx = [[1 | rm_tx_noun], 0 | 0]

    {:ok, spawn} =
      Router.start_engine(
        anode.router,
        Worker,
        {id, {:cairo, executor_tx}, env, topic, nil}
      )

    Ordering.new_order(env.ordering, [
      Anoma.Transaction.new_with_order(0, id, spawn)
    ])
    Router.send_raw(spawn, {:write_ready, 0})

    Mempool.execute(anode.mempool)

    TestHelper.Worker.wait_for_worker(spawn, :ok)

    assert {:ok, true} = Storage.get(anode.storage, ["rm", "commitment_root", anchor])

    :ok = Router.call(anode.router, {:unsubscribe_topic, topic, :local})

    anode
  end

  @spec execute_invalid_transaction() ::  Node.t()
  def execute_invalid_transaction() do

    anode = EStorage.august_node("invalid")

    snapshot_path = [:my_special_nock_snaphsot | 0]

    env = %Nock{snapshot_path: snapshot_path, ordering: anode.ordering}

    id = System.unique_integer([:positive])

    {:ok, topic} = Router.new_topic(anode.router)
    :ok = Router.call(anode.router, {:subscribe_topic, topic, :local})
    Storage.ensure_new(anode.storage)
    Ordering.reset(anode.ordering)

    {_ct, _merkle_proof, anchor} = ECommitmentTree.a_merkle_proof()

    # Insert the current root to the storage
    Storage.put(anode.storage, ["rm", "commitment_root", anchor], true)

    shielded_tx = EShieldedTransaction.a_invalid_shielded_transaction()

    rm_tx_noun = Noun.Nounable.to_noun(shielded_tx)
    executor_tx = [[1 | rm_tx_noun], 0 | 0]

    {:ok, spawn} =
      Router.start_engine(
        anode.router,
        Worker,
        {id, {:cairo, executor_tx}, env, topic, nil}
      )

    Ordering.new_order(env.ordering, [
      Anoma.Transaction.new_with_order(0, id, spawn)
    ])
    Router.send_raw(spawn, {:write_ready, 0})

    Mempool.execute(anode.mempool)

    TestHelper.Worker.wait_for_worker(spawn, :error)

    assert {:ok, true} = Storage.get(anode.storage, ["rm", "commitment_root", anchor])

    :ok = Router.call(anode.router, {:unsubscribe_topic, topic, :local})

    anode
  end

  @spec execute_double_transaction() ::  Node.t()
  def execute_double_transaction() do

    anode = EStorage.august_node("double")

    snapshot_path = [:my_special_nock_snaphsot | 0]

    env = %Nock{snapshot_path: snapshot_path, ordering: anode.ordering}

    id = System.unique_integer([:positive])

    {:ok, topic} = Router.new_topic(anode.router)
    :ok = Router.call(anode.router, {:subscribe_topic, topic, :local})
    Storage.ensure_new(anode.storage)
    Ordering.reset(anode.ordering)

    {_ct, _merkle_proof, anchor} = ECommitmentTree.a_merkle_proof()

    # Insert the current root to the storage
    Storage.put(anode.storage, ["rm", "commitment_root", anchor], true)

    shielded_tx = EShieldedTransaction.another_shielded_transaction()

    rm_tx_noun = Noun.Nounable.to_noun(shielded_tx)
    executor_tx = [[1 | rm_tx_noun], 0 | 0]

    {:ok, spawn} =
      Router.start_engine(
        anode.router,
        Worker,
        {id, {:cairo, executor_tx}, env, topic, nil}
      )

    Ordering.new_order(env.ordering, [
      Anoma.Transaction.new_with_order(0, id, spawn)
    ])
    Router.send_raw(spawn, {:write_ready, 0})

    Mempool.execute(anode.mempool)

    TestHelper.Worker.wait_for_worker(spawn, :ok)

    assert {:ok, true} = Storage.get(anode.storage, ["rm", "commitment_root", anchor])

    :ok = Router.call(anode.router, {:unsubscribe_topic, topic, :local})

    anode
  end
end
