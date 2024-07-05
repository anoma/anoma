defmodule Examples.ENode.EStorage do
  use Memoize

  require ExUnit.Assertions
  import ExUnit.Assertions

  alias Anoma.Identity.Backend.Memory
  alias Anoma.Identity.{Manager, Name, Evidence, SignsFor, Verification}
  alias Anoma.Crypto.Id
  alias Anoma.Symbol
  alias Anoma.Node
  alias Anoma.Node.{Storage, Router, Identity.Commitment}

  alias Examples.{ENock, ENode, EIdentity, ECrypto}
  ####################################################################
  ##                Keys for All Modules: Please use me             ##
  ##  Keys can also be found in the Noun section in the ENock file. ##
  ####################################################################

  def snapshot_point() do
    ENode.base_snapshot_path() |> hd()
  end

  def devil_value(), do: 444
  def lucky_value(), do: 777
  def lucky_key(), do: 777
  def devil_key(), do: 666

  def august_space(), do: ~w"August Water"
  def august_namespace(), do: :august_manager_space

  @spec august_subnamespace(Router.addr()) :: Name.t()
  def august_subnamespace(storage) do
    %Name{keyspace: august_namespace(), storage: storage}
  end

  def isuzumi_key(), do: august_space() ++ ["Izumi"]
  def miki_key(), do: august_space() ++ ["Miki"]

  defmemo random_id() do
    System.unique_integer([:positive])
  end

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

  @doc """
  I am the august storage, see my Keys for details on my storage setup

  ### Keys:
  - `miki_key/0` - `lucky_value/0`
  - `isuzumi_key/0` - `devil_value/0`
  - `Examples.ENode.base_snapshot_path/0` - snapshot of both
  """
  @spec august_node() :: Node.t()
  @spec august_node(Symbol.s()) :: Node.t()
  def august_node(storage_name \\ "august_node") do
    anode = empty_storage(storage_name)
    Storage.put(anode.storage, miki_key(), lucky_value())
    Storage.put(anode.storage, isuzumi_key(), devil_value())
    Storage.put_snapshot(anode.storage, snapshot_point())

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
  ##                    Properly namespaced storage                 ##
  ##   Differs from `august_node/1`: go through the name manager.   ##
  ####################################################################

  @spec reserved_august() ::
          {Node.t(), Memory.t(), Manager.instance(), Id.Extern.t()}
  @spec reserved_august(Symbol.s()) ::
          {Node.t(), Memory.t(), Manager.instance(), Id.Extern.t()}
  def reserved_august(storage_name \\ "reserved_august") do
    full = {anode, _mem, eng, pub} = EIdentity.memory_storage(storage_name)
    sub_space = august_subnamespace(anode.storage)

    august = august_space() |> hd

    assert {:ok, commited} = Commitment.commit(eng.commitment, august)

    name = [Name.name_space(), august]

    assert :ok == Name.reserve_namespace(sub_space, august, pub, commited)

    assert :improper_data ==
             Name.reserve_namespace(sub_space, "Alice", pub, commited)

    assert :already_there ==
             Name.reserve_namespace(sub_space, august, pub, commited)

    assert [{name, pub}] == Storage.get_keyspace(anode.storage, name)

    full
  end

  @doc """
  I am like `august_node/1`. Except I sign the namespace reserved

  Further besides the main space being owned by memory backed space,
  we have `ECrypto.londo/0` owning the water. Thankfully we aren't on
  Narn.

  """
  @spec august_node_proper() ::
          {Node.t(), Memory.t(), Manager.instance(), Id.Extern.t()}
  @spec august_node_proper(Symbol.s()) ::
          {Node.t(), Memory.t(), Manager.instance(), Id.Extern.t()}
  def august_node_proper(storage_name \\ "august_node_proper") do
    full = {anode, _mem, eng, pub} = reserved_august(storage_name)
    subnamespace = august_subnamespace(anode.storage)
    londo_pub = ECrypto.londo().external
    londo_space = {august_space(), londo_pub}

    assert {:ok, attestation} = Commitment.commit(eng.commitment, londo_space)
    assert :ok == Name.add(subnamespace, attestation, londo_space)
    assert :already_there == Name.add(subnamespace, attestation, londo_space)

    assert :no_namespace ==
             Name.add(subnamespace, attestation, {["Foo", "Bar"], londo_pub})

    assert MapSet.new([londo_pub, pub]) ==
             Name.all_identities(subnamespace, august_space() |> hd)

    full
  end

  @spec londo_speaks_for_alice() :: Node.t()
  @spec londo_speaks_for_alice(Symbol.s()) :: Node.t()
  def londo_speaks_for_alice(storage_name \\ "londo_speaks") do
    anode = empty_storage(storage_name)
    apid = EIdentity.alice_commits()

    apub = ECrypto.alice().external
    lpub = ECrypto.londo().external
    bpub = ECrypto.bertha().external

    assert {:ok, signed_key} = Commitment.commit(apid, lpub)

    assert SignsFor.sign_for(anode.storage, %Evidence{
             signature_key: apub,
             signed_data: lpub,
             signature: signed_key
           }) == :ok

    assert SignsFor.sign_for(anode.storage, %Evidence{
             signature_key: apub,
             signed_data: bpub,
             signature: signed_key
           }) == :key_not_verified

    assert SignsFor.known(anode.storage, apub) == MapSet.new([lpub])
    assert SignsFor.known(anode.storage, lpub) == MapSet.new([])
    assert SignsFor.signs_for?(anode.storage, apub, lpub)
    refute SignsFor.signs_for?(anode.storage, apub, bpub)
    anode
  end

  @spec bertha_speaks_for_all() :: Node.t()
  @spec bertha_speaks_for_all(Symbol.s()) :: Node.t()
  def bertha_speaks_for_all(storage_name \\ "bertha_speaks") do
    anode = londo_speaks_for_alice(storage_name)
    apub = ECrypto.alice().external
    lpub = ECrypto.londo().external
    bpub = ECrypto.bertha().external
    lpid = EIdentity.londo_commits()
    bpid = EIdentity.bertha_commits()

    assert {:ok, signed_key} = Commitment.commit(lpid, bpub)

    assert SignsFor.sign_for(anode.storage, %Evidence{
             signature_key: lpub,
             signed_data: bpub,
             signature: signed_key
           }) == :ok

    {:ok, signed} = Commitment.commit(bpid, <<3>>)
    {:ok, signed_blob} = Commitment.commit_combined(bpid, <<3>>)
    assert Verification.verify_request(signed, <<3>>, apub, anode.storage)
    assert Verification.verify_combined(signed_blob, apub, anode.storage)
    anode
  end

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
