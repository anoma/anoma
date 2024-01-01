defmodule AnomaTest.Node.Mempool do
  use ExUnit.Case

  alias Anoma.Storage
  alias Anoma.Node.Storage.Communicator, as: Scom
  alias Anoma.Node.Executor.Communicator, as: Ccom
  alias Anoma.Node.Mempool.Communicator, as: Mcom
  import TestHelper.Nock

  setup_all do
    storage = %Anoma.Storage{
      qualified: AnomaTest.Mempool.Qualified,
      order: AnomaTest.Mempool.Order
    }

    ordering = :mempool_storage_com
    executor = :mempool_test_com
    mempool = :mempool_com

    snapshot_path = [:my_special_nock_snaphsot | 0]
    env = %Nock{snapshot_path: snapshot_path, ordering: ordering}

    unless Process.whereis(ordering) do
      Anoma.Node.Storage.start_link(name: :mempool_storage, table: storage)
    end

    unless Process.whereis(executor) do
      Anoma.Node.Executor.start_link(
        env
        |> Map.to_list()
        |> Keyword.put(:name, :mempool_test)
      )
    end

    unless Process.whereis(mempool) do
      Anoma.Node.Mempool.start_link(
        name: :mempool,
        block_storage: :mempool_blocks,
        ordering: ordering,
        executor: executor
      )
    end

    # setup storage
    Mcom.hard_reset(mempool)
    [env: env, exec: executor, mem: mempool]
  end

  test "successful process", %{env: env, exec: executor, mem: mempool} do
    key = 555
    storage = Scom.get_storage(env.ordering)
    increment = increment_counter_val(key)
    zero = zero_counter(key)

    Ccom.subscribe(executor, self())
    Mcom.hard_reset(mempool)

    pid_zero = Mcom.tx(mempool, zero).pid

    Mcom.execute(mempool)
    pid_one = Mcom.tx(mempool, increment).pid
    pid_two = Mcom.tx(mempool, increment).pid

    Mcom.execute(mempool)
    assert_receive {:"$gen_cast", {:process_done, ^pid_zero}}
    assert_receive {:"$gen_cast", {:process_done, ^pid_one}}
    assert_receive {:"$gen_cast", {:process_done, ^pid_two}}
    assert {:ok, 2} = Storage.get(storage, key)
    Ccom.reset(executor)
  end

  test "Processes still snapshots", %{env: env, exec: executor, mem: mempool} do
    key = 555
    storage = Scom.get_storage(env.ordering)
    increment = increment_counter_val(key)

    Ccom.subscribe(executor, self())
    Mcom.hard_reset(mempool)

    pid_one = Mcom.tx(mempool, increment).pid
    pid_two = Mcom.tx(mempool, increment).pid

    Mcom.execute(mempool)
    assert_receive {:"$gen_cast", {:process_done, ^pid_one}}
    assert_receive {:"$gen_cast", {:process_done, ^pid_two}}
    assert :absent = Storage.get(storage, key)
    Ccom.reset(executor)
  end

  test "Processes gets killed", %{env: env, exec: _executor, mem: mempool} do
    key = 333
    storage = Scom.get_storage(env.ordering)

    Mcom.hard_reset(mempool)

    pid_zero = Mcom.tx(mempool, zero_counter(key)).pid

    Mcom.soft_reset(mempool)

    Mcom.execute(mempool)

    assert not Process.alive?(pid_zero)
    assert :absent = Storage.get(storage, key)
  end
end
