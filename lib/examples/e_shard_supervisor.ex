defmodule Anoma.Node.Examples.EShardSupervisor do
  @moduledoc """
  I contain examples demonstrating the ShardSupervisor functionality.
  """

  alias Anoma.Node
  alias Anoma.Node.Examples.ENode
  alias Anoma.Node.Registry
  alias Anoma.Node.Transaction.Shard
  alias Anoma.Node.Transaction.Shard.Cell
  alias Anoma.Tables
  alias Anoma.Node.Transaction.Shard.Supervisor

  import ExUnit.Assertions

  @doc """
  I test starting a node with a shard configuration, verifying that the
  ShardSupervisor starts the correct Shard processes via async setup,
  and that the router correctly maps keys to shard names.

  I create a unique node ID on each call to avoid state collision.
  """
  @spec shard_start_abc(String.t()) :: String.t()
  def shard_start_abc(node_id \\ Node.example_random_id()) do
    schema = [{["a"], 5}, ["b"], {["c"], 15}]
    shard_config = [strategy: :one_per_key, schema: schema]
    opts = [node_id: node_id, transaction: [shards: shard_config]]

    ENode.start_node(opts)

    # Check live Processes
    pid_a = Registry.whereis(node_id, Shard, :a)
    pid_b = Registry.whereis(node_id, Shard, :b)

    state_a = :sys.get_state(pid_a)
    state_b = :sys.get_state(pid_b)

    assert state_a.cells[["a"]].details[0].cell == %{value: 5},
           "Shard 'a' initial value mismatch at height 0"

    # Shard "b" should have no entry for key "b" at height 0
    b_cell = Map.get(state_b, ["b"], %Cell{})
    assert :empty == Cell.detail_at(b_cell, 0).cell

    # Check tables
    table = Tables.table_shard_key_map(node_id)

    assert read_table(table, ["a"]) == {:atomic, [{table, ["a"], :a}]},
           "Mnesia lookup for 'a' failed"

    assert read_table(table, ["d"]) == {:atomic, []},
           "Mnesia lookup for unknown key 'd' should return empty list"

    node_id
  end

  @doc """
  I'm like `shard_start_abc/1` but with d being 10
  """
  @spec shard_dynamically_abcd(String.t()) :: String.t()
  def shard_dynamically_abcd(node_id \\ Node.example_random_id()) do
    shard_start_abc(node_id)

    Supervisor.start_shard(node_id, ["d"], 10)

    pid = Registry.whereis(node_id, Shard, :d)
    assert is_pid(pid)

    table = Tables.table_shard_key_map(node_id)

    assert read_table(table, ["d"]) == {:atomic, [{table, ["d"], :d}]},
           "Mnesia lookup for 'd' failed"

    node_id
  end

  @doc """
  I'm like `shard_dynamically_abcd/1` but with e being empty
  """
  @spec shard_abcde(String.t()) :: String.t()
  def shard_abcde(node_id \\ Node.example_random_id()) do
    shard_dynamically_abcd(node_id)

    Supervisor.start_shard(node_id, ["e"])
    node_id
  end

  @spec read_table(atom(), term()) :: {:atomic, list()} | {:aborted, term()}
  def read_table(table, key) do
    :mnesia.transaction(fn -> :mnesia.read({table, key}) end)
  end
end
