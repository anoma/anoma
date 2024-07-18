defmodule Examples.ENode.EPinger do
  use Memoize

  require ExUnit.Assertions
  import ExUnit.Assertions

  alias Examples.{ENock, ENode, ENode.EStorage}
  alias Anoma.Symbol
  alias Anoma.Node
  alias Anoma.Node.{Router, Storage, Pinger}

  @spec pinger_run() :: Node.t()
  @spec pinger_run(Symbol.s()) :: Node.t()
  def pinger_run(storage_name \\ "pinger_run") do
    anode = anode(storage_name)
    zero = ENock.zero_counter(EStorage.miki_key())
    inc = ENock.increment_counter_val(EStorage.miki_key())
    ex_id = anode.executor_topic
    mem_t = anode.mempool_topic

    assert :ok == Router.call(anode.router, {:subscribe_topic, ex_id, :local})

    Pinger.set_timer(anode.pinger, 100)

    Pinger.start(anode.pinger)

    :ok = Router.call(anode.router, {:subscribe_topic, mem_t, :local})

    worker_zero =
      TestHelper.Mempool.wait_for_tx(anode.mempool, {:kv, zero}, 5000).addr

    TestHelper.Worker.wait_for_worker(worker_zero)

    worker_one =
      TestHelper.Mempool.wait_for_tx(anode.mempool, {:kv, inc}, 5000).addr

    worker_two =
      TestHelper.Mempool.wait_for_tx(anode.mempool, {:kv, inc}, 5000).addr

    Enum.each(
      [worker_one, worker_two],
      &TestHelper.Worker.wait_for_worker/1
    )

    assert {:ok, 2} = Storage.get(anode.storage, EStorage.miki_key())

    assert :ok ==
             Router.call(anode.router, {:unsubscribe_topic, ex_id, :local})

    assert :ok ==
             Router.call(anode.router, {:unsubscribe_topic, mem_t, :local})

    Anoma.Node.Pinger.set_timer(anode.pinger, :no_timer)

    anode
  end

  ####################################################################
  ##                             Phase 1                            ##
  ####################################################################

  @spec anode() :: Node.t()
  @spec anode(Symbol.s()) :: Node.t()
  @spec anode(Symbol.s(), Storage.t()) :: Node.t()
  def anode(arg \\ "none") do
    anode(arg, raw_storage(arg))
  end

  def anode(arg, storage) do
    storage
    |> ENode.fresh_full_node(Symbol.append(__MODULE__, "." <> to_string(arg)))
  end

  @spec raw_storage() :: Storage.t()
  @spec raw_storage(Symbol.s()) :: Storage.t()
  def raw_storage(arg \\ "none") do
    %Storage{
      qualified: Symbol.append(__MODULE__.Qualified, "." <> to_string(arg)),
      order: Symbol.append(__MODULE__.Order, "." <> to_string(arg))
    }
  end
end
