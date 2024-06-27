defmodule Examples.ENode.EStorage do
  use Memoize

  require ExUnit.Assertions
  import ExUnit.Assertions

  alias Anoma.Node.Router
  alias Examples.ENock
  alias Examples.ENode
  alias Anoma.Symbol
  alias Anoma.Node
  alias Anoma.Node.{Storage}

  ####################################################################
  ##                 Basic operations topics and snaps              ##
  ####################################################################

  @spec empty_storage() :: Node.t()
  @spec empty_storage(Symbol.s()) :: Node.t()
  def empty_storage(storage_name \\ "empty_storage") do
    anode = anode(storage_name)
    Storage.ensure_new(anode.storage)

    assert :absent == Storage.get(anode.storage, 555)
    assert [] == Storage.get_keyspace(anode.storage, [])
    assert [] == Storage.get_keyspace(anode.storage, ["aaaaaa"])
    assert [] == Storage.get_keyspace(anode.storage, ["aaaaaa", "bbbb"])

    anode
  end

  @spec put_storage() :: Node.t()
  @spec put_storage(Symbol.s()) :: Node.t()
  def put_storage(storage_name \\ "put_storage") do
    anode = empty_storage(storage_name)
    atom = ENock.one_two()

    sub(anode)

    Storage.put(anode.storage, atom, 1)

    assert_receive {:"$gen_cast",
                    {:router_cast, _, {:put, ^atom, 1, {:atomic, :ok}}}}

    assert {:ok, 1} = Storage.get(anode.storage, ENock.one_two())

    unsub(anode)

    anode
  end

  @doc """
  I have `Examples.Enock.one_two/2` in storage twice and a snapshot!

  The snapshot only snapshots the first key though, keep that in mind.
  """
  @spec snapshot_then_put() :: Node.t()
  @spec snapshot_then_put(Symbol.s()) :: Node.t()
  def snapshot_then_put(storage_name \\ "snapshot_then_put") do
    anode = put_storage(storage_name)
    luck = lucky_value()

    Storage.put_snapshot(anode.storage, snapshot_point())

    assert {:ok, snap} = Storage.get(anode.storage, snapshot_point())

    # Now the fun
    Storage.put(anode.storage, ENock.one_two(), luck)

    # snapshot assertions
    assert 1 = Storage.in_snapshot(snap, ENock.one_two()),
           "We should be able to find the position of the noun we wish to read"

    assert nil == Storage.in_snapshot(snap, ENock.nesting_noun()),
           "Nesting noun is not in storage, nil is the correct response"

    assert {:ok, 1} = Storage.get_at_snapshot(snap, ENock.one_two()),
           "The old value should be gotten from the snapshot"

    # querying assertions
    # rewrite the output of the function
    assert {:atomic, [{_, _, 2}]} =
             Storage.read_order_tx(anode.storage, ENock.one_two())

    assert {:atomic, [{_, _, 1}]} =
             Storage.read_at_order_tx(anode.storage, ENock.one_two(), 1)

    assert {:atomic, [{_, _, ^luck}]} =
             Storage.read_at_order_tx(anode.storage, ENock.one_two(), 2)

    anode
  end

  @spec snapshot_again() :: Node.t()
  @spec snapshot_again(Symbol.s()) :: Node.t()
  def snapshot_again(storage_name \\ "snapshot_again") do
    anode = snapshot_then_put(storage_name)

    # let's snapshot again
    Storage.put_snapshot(anode.storage, snapshot_point())

    assert {:ok, snap} = Storage.get(anode.storage, snapshot_point())

    assert {:ok, lucky_value()} ==
             Storage.get_at_snapshot(snap, ENock.one_two()),
           "Snapshotting should get the latest"

    assert {:ok, old_snap} =
             Storage.blocking_read(anode.storage, [1, snapshot_point() | 0]),
           "We can get the old snap, with a blocking read"

    assert {:ok, 1} = Storage.get_at_snapshot(old_snap, ENock.one_two()),
           "reading from an old snapshot, should give us the old value"

    anode
  end

  @spec deleting_the_put() :: Node.t()
  @spec deleting_the_put(Symbol.s()) :: Node.t()
  def deleting_the_put(storage_name \\ "deleting_the_put") do
    anode = snapshot_then_put(storage_name)

    Storage.delete_key(anode.storage, ENock.one_two())

    assert :absent = Storage.get(anode.storage, ENock.one_two()),
           "Key should no longer be there"

    assert {:ok, lucky_value()} ==
             Storage.blocking_read(anode.storage, [2, ENock.one_two() | 0]),
           "However, the old value should still be there!!!"

    anode
  end

  @spec deleting_nothing_works_fine() :: Node.t()
  @spec deleting_nothing_works_fine(Symbol.s()) :: Node.t()
  def deleting_nothing_works_fine(stg_name \\ "deleting_nothing_works_fine") do
    anode = empty_storage(stg_name)
    Storage.delete_key(anode.storage, 222)
    assert :absent = Storage.get(anode.storage, 222)
    anode
  end

  @spec blocking_for_put() :: Node.t()
  @spec blocking_for_put(Symbol.s()) :: Node.t()
  def blocking_for_put(storage_name \\ "blocking_for_put") do
    anode = empty_storage(storage_name)
    home = self()

    pid =
      spawn(fn ->
        assert {:ok, value} =
                 Storage.blocking_read(anode.storage, [
                   1,
                   ENock.nesting_noun() | 0
                 ])

        send(home, {:read, value})
      end)

    assert Process.alive?(pid) == true
    Storage.put(anode.storage, ENock.nesting_noun(), 1)
    assert_receive {:read, 1}, 100

    anode
  end

  @spec august_node() :: Node.t()
  @spec august_node(Symbol.s()) :: Node.t()
  def august_node(storage_name \\ "august_node") do
    anode = empty_storage(storage_name)
    Storage.put(anode.storage, miki_key(), lucky_value())
    Storage.put(anode.storage, isuzumi_key(), devil_value())

    izumi = {isuzumi_key(), devil_value()}
    miki = {miki_key(), lucky_value()}
    values = Enum.sort([miki, izumi])

    assert values ==
             Enum.sort(Storage.get_keyspace(anode.storage, august_space()))

    assert values ==
             Enum.sort(Storage.get_keyspace(anode.storage, ["August"]))

    assert [izumi] ==
             Enum.sort(Storage.get_keyspace(anode.storage, isuzumi_key()))

    anode
  end

  ####################################################################
  ##                      Some Paths and Owners                     ##
  ####################################################################
  def snapshot_point() do
    ENode.base_snapshot_path() |> hd()
  end

  def devil_value(), do: 444
  def lucky_value(), do: 777

  def august_space(), do: ~w"August Water"

  def isuzumi_key(), do: august_space() ++ ["Izumi"]
  def miki_key(), do: august_space() ++ ["Miki"]

  ####################################################################
  ##                    Helpers please factor out                   ##
  ####################################################################

  @spec sub(Node.t()) :: any()
  def sub(anode) do
    assert :ok ==
             anode.router
             |> Router.call({:subscribe_topic, anode.storage_topic, :local})
  end

  @spec unsub(Node.t()) :: any()
  def unsub(anode) do
    assert :ok =
             Router.call(
               anode.router,
               {:unsubscribe_topic, anode.storage_topic, :local}
             )
  end

  ####################################################################
  ##                             Phase 1                            ##
  ####################################################################

  @spec anode() :: Node.t()
  @spec anode(Symbol.s()) :: Node.t()
  defmemo anode(arg \\ "none") do
    raw_storage(arg)
    |> ENode.simple_storage_topic()
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
