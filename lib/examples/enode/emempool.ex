defmodule Examples.ENode.EMempool do
  use Memoize

  require ExUnit.Assertions
  import ExUnit.Assertions

  alias Examples.ENock
  alias Examples.ENode.EStorage
  alias Anoma.Symbol
  alias Anoma.Node
  alias Anoma.Node.{Router, Storage, Mempool}
  alias TestHelper.Mempool, as: MemHelp

  @spec increment_pool() :: Node.t()
  @spec increment_pool(Symbol.s()) :: Node.t()
  def increment_pool(storage_name \\ "increment_pool") do
    anode = EStorage.august_node(storage_name)

    Router.call(
      anode.router,
      {:subscribe_topic, anode.executor_topic, :local}
    )

    Router.call(anode.router, {:subscribe_topic, anode.mempool_topic, :local})

    inc = ENock.miki_increment_candidate()

    worker_inc = MemHelp.wait_for_tx(anode.mempool, {:kv, inc}).addr

    Mempool.execute(anode.mempool)

    TestHelper.Worker.wait_for_worker(worker_inc)

    assert {:ok, EStorage.lucky_value() + 1} ==
             Storage.get(anode.storage, EStorage.miki_key())

    Router.call(
      anode.router,
      {:unsubscribe_topic, anode.executor_topic, :local}
    )

    Router.call(
      anode.router,
      {:unsubscribe_topic, anode.mempool_topic, :local}
    )

    anode
  end

  @spec incremented_lucky_one_jam() :: Node.t()
  @spec incremented_lucky_one_jam(Symbol.s()) :: Node.t()
  def incremented_lucky_one_jam(storage_name \\ "incremented_lucky_one_jam") do
    anode = EStorage.snapshot_then_put(storage_name)

    Router.call(
      anode.router,
      {:subscribe_topic, anode.executor_topic, :local}
    )

    Router.call(anode.router, {:subscribe_topic, anode.mempool_topic, :local})

    jam = Nock.Jam.jam(ENock.increment_counter_val(ENock.one_two()))

    worker_inc = MemHelp.wait_for_tx(anode.mempool, {:kv, jam}).addr

    Mempool.execute(anode.mempool)

    TestHelper.Worker.wait_for_worker(worker_inc)

    assert {:ok, 2} == Storage.get(anode.storage, ENock.one_two())

    Router.call(
      anode.router,
      {:unsubscribe_topic, anode.executor_topic, :local}
    )

    Router.call(
      anode.router,
      {:unsubscribe_topic, anode.mempool_topic, :local}
    )

    anode
  end

  ####################################################################
  ##                             Phase 1                            ##
  ####################################################################
  # Use storage
end
