defmodule AnomaTest.Nock do
  use ExUnit.Case, async: true

  import Nock
  import TestHelper.Nock
  alias Anoma.{Storage, Order}
  alias Anoma.Node.Storage.Ordering

  doctest(Nock)

  setup_all do
    storage = %Anoma.Storage{
      qualified: AnomaTest.Nock.Qualified,
      order: AnomaTest.Nock.Order
    }

    {:ok, router} = Anoma.Node.Router.start()
    # on_exit(fn -> Anoma.Node.Router.stop(router.id) end)
    {:ok, ordering} =
      Anoma.Node.Router.start_engine(router, Anoma.Node.Storage.Ordering, %{
        table: storage
      })

    snapshot_path = [:my_special_nock_snaphsot | 0]

    env = %Nock{snapshot_path: snapshot_path, ordering: ordering}

    [env: env]
  end

  describe "Basic functionality" do
    test "base call" do
      assert nock(using_dec_core(), [9, 2, 0 | 1]) == {:ok, 998}
    end

    test "call with changing arguments" do
      assert nock(using_dec_core(), [9, 2, 10, [6, 1 | 5], 0 | 1]) == {:ok, 4}
    end
  end

  describe "Standard Library" do
    test "calling fib" do
      assert nock(factorial(), [9, 2, 10, [6, 1 | 7], 0 | 1]) == {:ok, 13}
    end
  end

  describe "Scrying" do
    test "successful scry", %{env: env} do
      key = 777
      id = System.unique_integer([:positive])
      storage = Ordering.get_storage(env.ordering)
      {:ok, increment} = nock(increment_counter_val(key), [9, 2, 0 | 1], env)

      Storage.ensure_new(storage)

      # setup id in the system for snapshot 1
      Ordering.new_order(env.ordering, [Order.new(1, id, self())])
      # put the key with some value
      Storage.put(storage, key, 5)
      # Now snapshot it so we can scry
      Storage.put_snapshot(storage, hd(env.snapshot_path))
      assert {:ok, val} = nock(increment, [9, 2, 10, [6, 1 | id], 0 | 1], env)
      assert val == [777 | 6]
    end

    test "scry may return error if not found", %{env: env} do
      key = 666
      id = System.unique_integer([:positive])
      storage = Ordering.get_storage(env.ordering)
      {:ok, increment} = nock(increment_counter_val(key), [9, 2, 0 | 1], env)

      Storage.ensure_new(storage)

      # setup id in the system for snapshot 1
      Ordering.new_order(env.ordering, [Order.new(1, id, self())])
      # Now snapshot it so we can scry
      Storage.put_snapshot(storage, hd(env.snapshot_path))
      assert :error = nock(increment, [9, 2, 10, [6, 1 | id], 0 | 1], env)
    end
  end
end
