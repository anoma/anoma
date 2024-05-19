defmodule AnomaTest.Storage do
  use ExUnit.Case

  alias Anoma.Node.Storage
  alias Anoma.Node.Router

  doctest(Anoma.Node.Storage)

  setup_all do
    {:ok, router, _} = Router.start()

    {:ok, topic} = Router.new_topic(router)

    storage = %Storage{
      qualified: AnomaTest.Storage.Qualified,
      order: AnomaTest.Storage.Order,
      topic: topic
    }

    {:ok, storage} =
      Router.start_engine(router, Storage, storage)

    Storage.ensure_new(storage)
    [storage: storage, router: router, topic: topic]
  end

  describe "Direct API" do
    test "Empty Storage is absent", %{storage: storage} do
      testing_atom = :QWERTZ_abc
      assert Storage.get(storage, testing_atom) == :absent
    end

    test "Putting works", %{storage: storage, router: router, topic: topic} do
      testing_atom = :QWERTZ_putting

      :ok = Router.call(router, {:subscribe_topic, topic, :local})

      Storage.put(storage, testing_atom, 1)

      assert {:ok, 1} = Storage.get(storage, testing_atom)

      assert_receive {:"$gen_cast",
                      {:router_cast, _,
                       {:put, ^testing_atom, 1, {:atomic, :ok}}}}

      :ok = Router.call(router, {:unsubscribe_topic, topic, :local})
    end

    test "Deleting key works", %{
      storage: storage,
      router: router,
      topic: topic
    } do
      testing_atom = :QWERTZ_delete

      :ok = Router.call(router, {:subscribe_topic, topic, :local})

      Storage.put(storage, testing_atom, 1)
      Storage.put(storage, testing_atom, 2)

      assert {:ok, 2} = Storage.get(storage, testing_atom)

      assert_receive {:"$gen_cast",
                      {:router_cast, _,
                       {:put, ^testing_atom, 1, {:atomic, :ok}}}}

      assert_receive {:"$gen_cast",
                      {:router_cast, _,
                       {:put, ^testing_atom, 2, {:atomic, :ok}}}}

      Storage.delete_key(storage, testing_atom)

      assert :absent = Storage.get(storage, testing_atom)

      assert_receive {:"$gen_cast",
                      {:router_cast, _,
                       {:put, ^testing_atom, :absent, {:atomic, :ok}}}}

      :ok = Router.call(router, {:unsubscribe_topic, topic, :local})
    end

    test "Deleting key again", %{
      storage: storage,
      router: router,
      topic: topic
    } do
      testing_atom = :QWERTZ_delete

      :ok = Router.call(router, {:subscribe_topic, topic, :local})

      Storage.put(storage, testing_atom, 1)
      Storage.put(storage, testing_atom, 2)

      assert {:ok, 2} = Storage.get(storage, testing_atom)

      assert_receive {:"$gen_cast",
                      {:router_cast, _,
                       {:put, ^testing_atom, 1, {:atomic, :ok}}}}

      assert_receive {:"$gen_cast",
                      {:router_cast, _,
                       {:put, ^testing_atom, 2, {:atomic, :ok}}}}

      Storage.delete_key(storage, testing_atom)
      Storage.delete_key(storage, testing_atom)

      assert :absent = Storage.get(storage, testing_atom)

      assert_receive {:"$gen_cast",
                      {:router_cast, _,
                       {:put, ^testing_atom, :absent, {:atomic, :ok}}}}

      :ok = Router.call(router, {:unsubscribe_topic, topic, :local})
    end

    test "block_reads work", %{storage: storage, router: router, topic: topic} do
      testing_atom = :QWERTZ_blocking

      :ok = Router.call(router, {:subscribe_topic, topic, :local})

      Storage.put(storage, testing_atom, 1)

      assert {:ok, block} =
               Storage.blocking_read(storage, [1, testing_atom | 0])

      assert {:ok, current} = Storage.get(storage, testing_atom)

      assert_receive {:"$gen_cast",
                      {:router_cast, _,
                       {:put, ^testing_atom, 1, {:atomic, :ok}}}}

      assert current == block

      :ok = Router.call(router, {:unsubscribe_topic, topic, :local})
    end

    test "block_reads can read the past", %{
      storage: storage,
      router: router,
      topic: topic
    } do
      testing_atom = :QWERTZ_blocking_past

      :ok = Router.call(router, {:subscribe_topic, topic, :local})

      Storage.put(storage, testing_atom, 1)
      Storage.put(storage, testing_atom, 2)

      assert {:ok, bl_1} =
               Storage.blocking_read(storage, [1, testing_atom | 0])

      assert {:ok, bl_2} =
               Storage.blocking_read(storage, [2, testing_atom | 0])

      assert {:ok, curr} = Storage.get(storage, testing_atom)

      assert_receive {:"$gen_cast",
                      {:router_cast, _,
                       {:put, ^testing_atom, 1, {:atomic, :ok}}}}

      assert_receive {:"$gen_cast",
                      {:router_cast, _,
                       {:put, ^testing_atom, 2, {:atomic, :ok}}}}

      assert curr == bl_2
      assert bl_1 + 1 == bl_2

      :ok = Router.call(router, {:unsubscribe_topic, topic, :local})
    end

    test "blocking_reads must accept position indicators", %{storage: s} do
      assert Storage.blocking_read(s, :Centuri_Republic) == :error
    end

    test "blocking_reads really do block", %{storage: storage} do
      testing_atom = System.unique_integer()
      home = self()

      pid =
        spawn(fn ->
          assert {:ok, value} =
                   Storage.blocking_read(storage, [1, testing_atom | 0])

          send(home, {:read, value})
        end)

      assert Process.alive?(pid) == true
      Storage.put(storage, testing_atom, 1)
      assert_receive {:read, 1}, 100
      assert Process.alive?(pid) == false
    end
  end

  describe "Querying by hand" do
    test "Reading at a known order gives results", %{storage: storage} do
      testing_atom = 750_089_999
      Storage.write_at_order_tx(storage, testing_atom, 10, 3)

      assert Storage.read_at_order_tx(storage, testing_atom, 3) ==
               {:atomic,
                [
                  {Router.Engine.get_state(storage).qualified,
                   [3, testing_atom | 0], 10}
                ]}
    end

    test "Writing at an order gives us the same testing order", %{
      storage: storage
    } do
      testing_atom = 999_888_777_666
      Storage.write_at_order_tx(storage, testing_atom, 10, 3)

      assert Storage.read_order_tx(storage, testing_atom) ==
               {:atomic,
                [{Router.Engine.get_state(storage).order, testing_atom, 3}]}
    end
  end

  describe "Namespaces" do
    test "Properly get the put keys", %{storage: storage} do
      key_space = ["foo", "bar"]
      key_1 = key_space ++ ["baz"]
      key_2 = key_space ++ ["faz"]
      Storage.put(storage, key_1, 1)
      Storage.put(storage, key_2, 1)
      key_1_res = {["foo", "bar", "baz"], 1}
      key_2_res = {["foo", "bar", "faz"], 1}

      assert Enum.sort([key_1_res, key_2_res]) ==
               Enum.sort(Storage.get_keyspace(storage, ["foo"]))

      assert Enum.sort([key_1_res, key_2_res]) ==
               Enum.sort(Storage.get_keyspace(storage, ["foo", "bar"]))

      assert [key_1_res] ==
               Storage.get_keyspace(storage, ["foo", "bar", "baz"])
    end

    test "no names no query", %{storage: storage} do
      assert [] == Storage.get_keyspace(storage, ["aaaaaa", "bbbb"])
    end
  end

  describe "Snapshots" do
    test "snapshots properly put", %{
      storage: storage,
      router: router,
      topic: topic
    } do
      snapshot_storage = :snapshot_super_secret

      :ok = Router.call(router, {:subscribe_topic, topic, :local})

      Storage.put_snapshot(storage, snapshot_storage)
      assert {:ok, _} = Storage.get(storage, snapshot_storage)

      assert_receive {:"$gen_cast",
                      {:router_cast, _,
                       {:put, ^snapshot_storage, _, {:atomic, :ok}}}}

      :ok = Router.call(router, {:unsubscribe_topic, topic, :local})
    end

    test "snapshots properly get the latest", %{
      storage: storage,
      router: router,
      topic: topic
    } do
      snapshot_storage = :super_hot
      testing_atom = 111_222_333_444_555_666

      :ok = Router.call(router, {:subscribe_topic, topic, :local})

      Storage.write_at_order_tx(storage, testing_atom, 10, 3)
      Storage.put_snapshot(storage, snapshot_storage)

      assert_receive {:"$gen_cast",
                      {:router_cast, _,
                       {:write, ^testing_atom, 10, 3, {:atomic, :ok}}}}

      assert_receive {:"$gen_cast",
                      {:router_cast, _,
                       {:put, ^snapshot_storage, _, {:atomic, :ok}}}}

      assert {:ok, snapshot} = Storage.get(storage, snapshot_storage)

      assert Storage.in_snapshot(snapshot, testing_atom) == 3

      assert Storage.get_at_snapshot(snapshot, testing_atom) ==
               {:ok, 10}

      :ok = Router.call(router, {:unsubscribe_topic, topic, :local})
    end

    test "missing key gives us nil", %{storage: storage} do
      nonsense_atom = :very_good_atom
      assert {:atomic, snapshot} = Storage.snapshot_order(storage)
      assert Storage.get_at_snapshot(snapshot, nonsense_atom)
    end
  end
end
