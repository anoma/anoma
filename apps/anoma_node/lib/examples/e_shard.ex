defmodule Anoma.Node.Examples.EShard do
  @moduledoc """
  I contain examples on how to interact with the Shard module.
  """

  alias Anoma.Node
  alias Anoma.Node.Registry
  alias Anoma.Tables
  alias Anoma.Node.Transaction.Shard
  alias Anoma.Node.Transaction.Shard.Detail
  alias Anoma.Node.Transaction.Shard.Cell
  alias Anoma.Node.Examples.EShardSupervisor

  import ExUnit.Assertions

  @spec abc_all_write_to_5(String.t()) :: String.t()
  def abc_all_write_to_5(node_id \\ Node.example_random_id()) do
    EShardSupervisor.shard_start_abc(node_id)

    pid_c = Registry.via(node_id, Shard, :c)
    Shard.advance_watermark(Registry.via(node_id, Shard, :a), ["a"], 5)
    Shard.advance_watermark(Registry.via(node_id, Shard, :b), ["b"], 5)
    Shard.advance_watermark(pid_c, ["c"], 5)

    assert %{write: 5} == :sys.get_state(pid_c).cells[["c"]].watermarks

    node_id
  end

  @spec reserve_and_read_abc_5(String.t()) :: String.t()
  def reserve_and_read_abc_5(node_id \\ Node.example_random_id()) do
    EShardSupervisor.shard_start_abc(node_id)

    shard_b = Registry.via(node_id, Shard, :b)
    shard_c = Registry.via(node_id, Shard, :c)

    # read before the fact, note watermark at 5, means we can read at 4
    read_task = Task.async(fn -> Shard.read(shard_b, ["b"], 4) end)
    assert Task.yield(read_task, 100) == nil

    abc_all_write_to_5(node_id)

    assert Task.await(read_task, 1000) == :absent
    assert Shard.read(shard_b, ["b"], 4) == :absent
    assert Shard.read(shard_c, ["c"], 4) == {:ok, 15}

    node_id
  end

  @spec abc_val_for_a_write_to_5(String.t()) :: String.t()
  def abc_val_for_a_write_to_5(node_id \\ Node.example_random_id()) do
    EShardSupervisor.shard_start_abc(node_id)

    shard_a = Registry.via(node_id, Shard, :a)

    # Kinds of reads that could happen in the past, present, and never
    read_before = Task.async(fn -> Shard.read(shard_a, ["a"], 3) end)
    read_after = Task.async(fn -> Shard.read(shard_a, ["a"], 4) end)
    read_fail = Task.async(fn -> Shard.read(shard_a, ["a"], 14) end)

    Shard.reserve(shard_a, ["a"], 3)

    abc_all_write_to_5(node_id)

    assert Task.await(read_before, 1000) == {:ok, 5}

    Shard.write(shard_a, ["a"], 55, 3)

    assert Task.await(read_after, 1000) == {:ok, 55}

    assert Process.alive?(read_fail.pid)
    Task.shutdown(read_fail, :brutal_kill)

    Shard.retract(shard_a, ["a"], 14, read_fail.pid)
    # A pending read stays, probably a bug?
    node_id
  end

  @spec abc_val_a_write_to_5_shard_a() :: map()
  def abc_val_a_write_to_5_shard_a() do
    %{
      ["a"] => %Cell{
        watermarks: %{write: 5},
        details: %{
          0 => %Detail{cell: %{value: 5}, pending: nil},
          3 => %Detail{cell: %{value: 55}},
          4 => %Detail{},
          14 => %Detail{}
        }
      }
    }
  end

  @spec abc_val_a_gc_start_write_30(String.t()) :: String.t()
  def abc_val_a_gc_start_write_30(node_id \\ Node.example_random_id()) do
    abc_val_for_a_write_to_5(node_id)

    shard_a = Registry.via(node_id, Shard, :a)

    assert :sys.get_state(shard_a).cells == abc_val_a_write_to_5_shard_a()

    Shard.advance_watermark(shard_a, ["a"], 30)
    assert Shard.read(shard_a, ["a"], 12) == {:ok, 55}

    node_id
  end

  @spec abc_val_acq_before_watermark(String.t()) :: String.t()
  def abc_val_acq_before_watermark(node_id \\ Node.example_random_id()) do
    abc_val_a_gc_start_write_30(node_id)

    shard_via = Registry.via(node_id, Shard, :a)

    assert Shard.reserve(shard_via, ["a"], 10) ==
             {:error, :reserving_write_under_write_watermark}

    node_id
  end

  @spec abc_val_rewrite_to_key_no_ff(String.t()) :: String.t()
  def abc_val_rewrite_to_key_no_ff(node_id \\ Node.example_random_id()) do
    abc_val_for_a_write_to_5(node_id)

    shard_a = Registry.via(node_id, Shard, :a)

    Shard.reserve(shard_a, ["a"], 20)
    Shard.write(shard_a, ["a"], "value_at_20", 20)

    assert Shard.reserve(shard_a, ["a"], 20) == {:error, :occupied}

    node_id
  end

  @spec abc_val_a_write_to_5_shard_a() :: map()
  def abc_val_a_waiting_7_11_shard_a() do
    %{
      ["a"] => %Cell{
        watermarks: %{write: 5},
        details: %{
          0 => %Detail{cell: %{value: 5}, pending: nil},
          3 => %Detail{cell: %{value: 55}},
          4 => %Detail{},
          7 => %Detail{cell: :reserved},
          11 => %Detail{cell: :reserved},
          14 => %Detail{}
        }
      }
    }
  end

  @spec abc_val_a_waiting_7_11(String.t()) :: String.t()
  def abc_val_a_waiting_7_11(node_id \\ Node.example_random_id()) do
    abc_val_for_a_write_to_5(node_id)

    shard_a = Registry.via(node_id, Shard, :a)

    Shard.reserve(shard_a, ["a"], 7)
    Shard.reserve(shard_a, ["a"], 11)

    assert :sys.get_state(shard_a).cells == abc_val_a_waiting_7_11_shard_a()

    node_id
  end

  @spec abc_val_a_double_waiting_write_7_11(String.t()) :: String.t()
  def abc_val_a_double_waiting_write_7_11(node_id \\ Node.example_random_id()) do
    abc_val_a_waiting_7_11(node_id)

    shard_a = Registry.via(node_id, Shard, :a)

    Shard.write(shard_a, ["a"], "Hi Life", 11)
    Shard.advance_watermark(shard_a, ["a"], 12)

    # We can read!!
    assert Shard.read(shard_a, ["a"], 12) == {:ok, "Hi Life"}
    # We can still write
    Shard.write(shard_a, ["a"], "Family Mart", 7)
    assert Shard.read(shard_a, ["a"], 8) == {:ok, "Family Mart"}

    node_id
  end

  # 7-11 won the monopoly, no Hi Life No Family Mart
  @spec abc_unresrve_the_7_11(String.t()) :: String.t()
  def abc_unresrve_the_7_11(node_id \\ Node.example_random_id()) do
    abc_val_a_waiting_7_11(node_id)

    shard_a = Registry.via(node_id, Shard, :a)

    current_state = %{
      ["a"] => %Cell{
        watermarks: %{write: 5},
        details: %{
          0 => %Detail{cell: %{value: 5}, pending: nil},
          3 => %Detail{cell: %{value: 55}},
          4 => %Detail{},
          7 => %Detail{cell: :empty},
          11 => %Detail{cell: :empty},
          14 => %Detail{}
        }
      }
    }

    Shard.unreserve(shard_a, ["a"], 7)
    Shard.unreserve(shard_a, ["a"], 11)

    assert :sys.get_state(shard_a).cells == current_state

    node_id
  end

  @spec abc_val_a_unresve_advances_7_11(String.t()) :: String.t()
  def abc_val_a_unresve_advances_7_11(node_id \\ Node.example_random_id()) do
    abc_val_a_waiting_7_11(node_id)

    shard_a = Registry.via(node_id, Shard, :a)

    read = Task.async(fn -> Shard.read(shard_a, ["a"], 12) end)
    Shard.advance_watermark(shard_a, ["a"], 12)

    Shard.write(shard_a, ["a"], "Family Mart", 7)

    Shard.unreserve(shard_a, ["a"], 11)
    # Unreserving should have made our read work
    assert Task.await(read, 1000) == {:ok, "Family Mart"}

    node_id
  end

  def backup_convinece_stores_a(node_id \\ Node.example_random_id()) do
    abc_val_a_double_waiting_write_7_11(node_id)

    shard_a = Registry.via(node_id, Shard, :a)

    table = Tables.table_shard_backups(node_id)

    expected_backup = [
      [{:a, ["a"], 0}, 5],
      [{:a, ["a"], 3}, 55],
      [{:a, ["a"], 7}, "Family Mart"],
      [{:a, ["a"], 11}, "Hi Life"]
    ]

    Shard.backup_state(shard_a)

    {:atomic, backup} =
      :mnesia.transaction(fn ->
        :mnesia.select(table, [{{table, :"$1", :"$2"}, [], [:"$$"]}])
      end)

    assert Enum.sort(backup) == Enum.sort(expected_backup)

    node_id
  end
end
