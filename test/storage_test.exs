defmodule AnomaTest.Storage do
  use ExUnit.Case

  alias Anoma.Storage

  doctest(Anoma.Storage)

  setup_all do
    # base storage testing default
    storage = %Storage{
      qualified: AnomaTest.Storage.Qualified,
      order: AnomaTest.Storage.Order
    }

    Storage.ensure_new(storage)
    [storage: storage]
  end

  describe "Direct API" do
    test "Empty Storage is absent", %{storage: storage} do
      testing_atom = :QWERTZ_abc
      assert Storage.get(storage, testing_atom) == :absent
    end

    test "Putting works", %{storage: storage} do
      testing_atom = :QWERTZ_putting
      assert {:atomic, :ok} = Storage.put(storage, testing_atom, 1)
      assert {:ok, 1} = Storage.get(storage, testing_atom)
    end

    test "block_reads work", %{storage: storage} do
      testing_atom = :QWERTZ_blocking
      assert {:atomic, :ok} = Storage.put(storage, testing_atom, 1)

      assert {:ok, block} =
               Storage.blocking_read(storage, [1, testing_atom | 0])

      assert {:ok, current} = Storage.get(storage, testing_atom)
      assert current == block
    end

    test "block_reads can read the past", %{storage: storage} do
      testing_atom = :QWERTZ_blocking_past
      assert {:atomic, :ok} = Storage.put(storage, testing_atom, 1)
      assert {:atomic, :ok} = Storage.put(storage, testing_atom, 2)

      assert {:ok, bl_1} =
               Storage.blocking_read(storage, [1, testing_atom | 0])

      assert {:ok, bl_2} =
               Storage.blocking_read(storage, [2, testing_atom | 0])

      assert {:ok, curr} = Storage.get(storage, testing_atom)
      assert curr == bl_2
      assert bl_1 + 1 == bl_2
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
      Storage.write_at_order(storage, testing_atom, 10, 3)

      assert Storage.read_at_order(storage, testing_atom, 3) ==
               {:atomic, [{storage.qualified, [3, testing_atom | 0], 10}]}
    end

    test "Writing at an order gives us the same testing order", %{
      storage: storage
    } do
      testing_atom = 999_888_777_666
      Storage.write_at_order(storage, testing_atom, 10, 3)

      assert Storage.read_order(storage, testing_atom) ==
               {:atomic, [{storage.order, testing_atom, 3}]}
    end
  end

  describe "Snapshots" do
    test "snapshots properly put", %{storage: storage} do
      snapshot_storage = :snapshot_super_secret
      assert {:atomic, :ok} = Storage.put_snapshot(storage, snapshot_storage)
      assert {:ok, _} = Storage.get(storage, snapshot_storage)
    end

    test "snapshots properly get the latest", %{storage: storage} do
      snapshot_storage = :super_hot
      testing_atom = 111_222_333_444_555_666
      Storage.write_at_order(storage, testing_atom, 10, 3)
      assert {:atomic, :ok} = Storage.put_snapshot(storage, snapshot_storage)
      assert {:ok, snapshot} = Storage.get(storage, snapshot_storage)
      assert Storage.in_snapshot(snapshot, testing_atom) == 3

      assert Storage.get_at_snapshot(snapshot, testing_atom) ==
               {:ok, 10}
    end

    test "missing key gives us nil", %{storage: storage} do
      nonsense_atom = :very_good_atom
      assert {:atomic, snapshot} = Storage.snapshot_order(storage)
      assert Storage.get_at_snapshot(snapshot, nonsense_atom)
    end
  end
end
