defmodule AnomaTest.Node.Storage do
  use ExUnit.Case, async: true

  alias Anoma.{Storage, Order}
  alias Anoma.Node.Storage.{Communicator, Ordering}

  doctest(Anoma.Node.Storage)
  doctest(Anoma.Node.Storage.Communicator)
  doctest(Anoma.Node.Storage.Ordering)

  setup_all do
    # base storage testing default
    storage = %Anoma.Storage{
      qualified: AnomaTest.Node.Storage.Qualified,
      order: AnomaTest.Node.Storage.Order
    }

    ordering = :node_storage_com

    unless Process.whereis(ordering) do
      Anoma.Node.Storage.start_link(name: :node_storage, table: storage)
    end

    [ordering: ordering]
  end

  test "reset works", %{ordering: ordering} do
    Communicator.new_order(ordering, [Order.new(1, <<3>>, self())])
    Communicator.reset(ordering)
    ordering = Communicator.state(ordering)
    assert ordering.hash_to_order == %{}
    assert ordering.next_order == 1
  end

  test "added to the hash_ordering", %{ordering: ordering} do
    Communicator.reset(ordering)

    Communicator.new_order(
      ordering,
      [Order.new(1, <<3>>, self()), Order.new(1, <<3>>, self())],
      false
    )

    ordering = Communicator.state(ordering)
    assert ordering.hash_to_order == %{<<3>> => 1}
  end

  test "getting proper offsets", %{ordering: ordering} do
    Communicator.reset(ordering)
    assert 1 == Communicator.next_order(ordering)

    Communicator.new_order(ordering, [
      Order.new(1, <<3>>, self()),
      Order.new(2, <<3>>, self())
    ])

    assert 3 == Communicator.next_order(ordering)
  end

  test "receiving read readies", %{ordering: ordering} do
    Communicator.new_order(ordering, [
      Order.new(3, <<3>>, self()),
      Order.new(4, <<3>>, self())
    ])

    assert_receive {:read_ready, 3}
    assert_receive {:read_ready, 4}
  end

  test "we properly cache orders", %{ordering: ordering} do
    Communicator.reset(ordering)
    assert Communicator.true_order(ordering, <<3>>) == nil
    Communicator.new_order(ordering, [Order.new(1, <<3>>, self())])
    assert Communicator.true_order(ordering, <<3>>) == 1
  end

  describe "User Blocking API" do
    test "properly get same snapshot", %{ordering: ordering} do
      sstore = :babylon_2
      testing_atom = System.unique_integer()
      storage = Communicator.get_storage(ordering)

      Communicator.reset(ordering)
      Storage.ensure_new(storage)

      Communicator.new_order(ordering, [Order.new(1, <<1>>, self())])
      Storage.put(storage, testing_atom, 1)
      Storage.put_snapshot(storage, sstore)
      Storage.put(storage, testing_atom, 2)
      Storage.put_snapshot(storage, sstore)

      assert {:ok, snapshot_1} =
               Ordering.caller_blocking_read_id(ordering, [<<1>>, sstore | 0])

      assert {:ok, snapshot_2} =
               Ordering.caller_blocking_read_id(ordering, [<<1>>, sstore | 0])

      assert {:ok, test_1} = Storage.get_at_snapshot(snapshot_1, testing_atom)

      assert {:ok, test_2} = Storage.get_at_snapshot(snapshot_2, testing_atom)

      assert test_1 == test_2
    end

    test "properly waits until it's turn", %{ordering: ordering} do
      sstore = :babylon_3
      testing_atom = System.unique_integer()
      storage = Communicator.get_storage(ordering)

      Communicator.reset(ordering)
      Storage.ensure_new(storage)

      home = self()

      waiting =
        spawn(fn ->
          assert {:ok, snapshot} =
                   Ordering.caller_blocking_read_id(ordering, [
                     <<1>>,
                     sstore | 0
                   ])

          assert {:ok, val} = Storage.get_at_snapshot(snapshot, testing_atom)

          send(home, {:received, val})
        end)

      Storage.put(storage, testing_atom, 1)
      Storage.put_snapshot(storage, sstore)

      assert Process.alive?(waiting)
      Communicator.new_order(ordering, [Order.new(1, <<1>>, waiting)])
      assert_receive {:received, 1}
    end

    test "properly get snapshot from id", %{ordering: ordering} do
      sstore = :babylon_4
      testing_atom = System.unique_integer()
      storage = Communicator.get_storage(ordering)

      Communicator.reset(ordering)
      Storage.ensure_new(storage)

      Communicator.new_order(ordering, [
        Order.new(1, <<1>>, self()),
        Order.new(2, <<2>>, self())
      ])

      Storage.put(storage, testing_atom, 1)
      Storage.put_snapshot(storage, sstore)
      Storage.put(storage, testing_atom, 2)
      Storage.put_snapshot(storage, sstore)

      assert {:ok, snapshot_1} =
               Ordering.caller_blocking_read_id(ordering, [<<1>>, sstore | 0])

      assert {:ok, snapshot_2} =
               Ordering.caller_blocking_read_id(ordering, [<<2>>, sstore | 0])

      assert {:ok, test_1} = Storage.get_at_snapshot(snapshot_1, testing_atom)

      assert {:ok, test_2} = Storage.get_at_snapshot(snapshot_2, testing_atom)

      assert test_1 + 1 == test_2
    end
  end
end
