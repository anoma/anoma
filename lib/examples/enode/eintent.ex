defmodule Examples.ENode.EIntent do
  use Memoize

  require ExUnit.Assertions
  import ExUnit.Assertions

  alias Examples.{ETransaction, ENode, ENode.EStorage}
  alias Anoma.Symbol
  alias Anoma.Node
  alias Anoma.Node.{Router, Storage, Mempool, IntentPool, Solver}

  @doc """
  I am a simple solver. The return state is the solved node

  We do not send any other transactions such as incrementing a solved
  counter, though we could easily submit it.
  """
  @spec solved_trade() :: Node.t()
  @spec solved_trade(Symbol.s()) :: Node.t()
  def solved_trade(storage_name \\ "solved_trade") do
    storage = Node.raw_storage(EStorage.empty_storage(storage_name))
    anode = anode(storage_name, storage)
    {solver, ip, solutions, anode} = ENode.solver(anode)

    :ok = Router.call(anode.router, {:subscribe_topic, solutions, :local})

    IntentPool.new_intent(ip, ETransaction.ax_for_y())
    IntentPool.new_intent(ip, ETransaction.by_for_x())

    :ok =
      Router.call(
        anode.router,
        {:subscribe_topic, anode.mempool_topic, :local}
      )

    assert_receive {:"$gen_cast", {_, _, {:solutions, [solution]}}}

    assert solution == ETransaction.full_x_for_y()

    Solver.mempool_send(solver, anode.mempool)

    assert_receive {:"$gen_cast", {_, _, {:submitted, _}}}

    assert {:ok, 1} = Mempool.execute(anode.mempool)

    assert_receive {:"$gen_cast", {:router_cast, _, {:executed, _}}}

    # We should assert this but Idk how to await for it.  We need to
    # figure out how to wait for the solution, then check the
    # rm_commitment = 1
    # (anode |> Node.raw_storage()).rm_commitments |> Mnesia.dirty_dump |> length

    :ok = Router.call(anode.router, {:unsubscribe_topic, solutions, :local})

    :ok =
      Router.call(
        anode.router,
        {:unsubscribe_topic, anode.mempool_topic, :local}
      )

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
