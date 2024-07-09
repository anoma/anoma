defmodule AnomaTest.Node.Storage do
  use TestHelper.TestMacro, async: true

  alias Anoma.Transaction
  alias Anoma.Node.Storage
  alias Anoma.Node.Ordering
  alias Anoma.Node.Router
  alias Anoma.Node.Router.Engine

  doctest(Anoma.Node.Ordering)

  setup_all do
    # base storage testing default
    storage = %Storage{
      qualified: AnomaTest.Node.Storage.Qualified,
      order: AnomaTest.Node.Storage.Order,
      namespace: [1, 2, 3]
    }

    {:ok, router, _} = Anoma.Node.Router.start()

    {:ok, storage} =
      Anoma.Node.Router.start_engine(router, Storage, storage)

    {:ok, ordering} =
      Anoma.Node.Router.start_engine(router, Ordering, %{
        storage: storage
      })

    [ordering: ordering, router: router]
  end

  test "reset works", %{ordering: ordering} do
    Ordering.new_order(ordering, [
      Transaction.new_with_order(1, <<3>>, Router.self_addr())
    ])

    Ordering.reset(ordering)
    ordering = Engine.get_state(ordering)
    assert ordering.hash_to_order == %{}
    assert ordering.next_order == 1
  end

  test "added to the hash_ordering", %{ordering: ordering} do
    Ordering.reset(ordering)

    Ordering.new_order(
      ordering,
      [
        Transaction.new_with_order(1, <<3>>, Router.self_addr()),
        Transaction.new_with_order(1, <<3>>, Router.self_addr())
      ]
    )

    ordering = Engine.get_state(ordering)
    assert ordering.hash_to_order == %{<<3>> => 1}
  end

  test "getting proper offsets", %{ordering: ordering} do
    Ordering.reset(ordering)
    assert 1 == Engine.get_state(ordering).next_order

    Ordering.new_order(ordering, [
      Transaction.new_with_order(1, <<3>>, Router.self_addr()),
      Transaction.new_with_order(2, <<3>>, Router.self_addr())
    ])

    assert 3 == Engine.get_state(ordering).next_order
  end

  test "receiving read readies", %{ordering: ordering} do
    Ordering.new_order(ordering, [
      Transaction.new_with_order(3, <<3>>, Router.self_addr()),
      Transaction.new_with_order(4, <<3>>, Router.self_addr())
    ])

    assert_receive {:read_ready, 3}
    assert_receive {:read_ready, 4}
  end

  test "we properly cache orders", %{ordering: ordering} do
    Ordering.reset(ordering)
    assert Ordering.true_order(ordering, <<3>>) == nil

    Ordering.new_order(ordering, [
      Transaction.new_with_order(1, <<3>>, Router.self_addr())
    ])

    assert Ordering.true_order(ordering, <<3>>) == 1
  end

  describe "User Blocking API" do
    test "properly get same snapshot", %{ordering: ordering} do
      sstore = :babylon_2
      testing_atom = System.unique_integer()
      storage = Engine.get_state(ordering).storage

      Ordering.reset(ordering)
      Storage.ensure_new(storage)

      Ordering.new_order(ordering, [
        Transaction.new_with_order(1, <<1>>, Router.self_addr())
      ])

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
      storage = Engine.get_state(ordering).storage

      Ordering.reset(ordering)
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

      Ordering.new_order(ordering, [
        Transaction.new_with_order(1, <<1>>, waiting)
      ])

      assert_receive({:received, 1}, 5000)
    end

    test "properly get snapshot from id", %{
      ordering: ordering
    } do
      sstore = :babylon_4
      testing_atom = System.unique_integer()
      storage = Engine.get_state(ordering).storage

      Ordering.reset(ordering)
      Storage.ensure_new(storage)

      Ordering.new_order(ordering, [
        Transaction.new_with_order(1, <<1>>, Router.self_addr()),
        Transaction.new_with_order(2, <<2>>, Router.self_addr())
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
