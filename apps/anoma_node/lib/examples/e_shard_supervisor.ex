defmodule Anoma.Node.Examples.EShardSupervisor do
  @moduledoc """
  I contain examples demonstrating the ShardSupervisor functionality.
  """

  alias Anoma.Node.Examples.ENode
  alias Anoma.Node.Registry
  alias Anoma.Node.Transaction.Shard
  alias Anoma.Node.Tables
  alias Anoma.Node.Transaction.Shard.Supervisor

  import ExUnit.Assertions

  @doc """
  I test starting a node with a shard configuration, verifying that the
  ShardSupervisor starts the correct Shard processes via async setup,
  and that the router correctly maps keys to shard names.

  I create a unique node ID on each call to avoid state collision.
  I return the started `ENode` struct.
  """
  @spec shard_supervisor_startup_and_routing() :: ENode.t()
  def shard_supervisor_startup_and_routing() do
    # Generate unique node ID for isolation
    node_id =
      "shard_sup_node_" <>
        (System.unique_integer([:positive]) |> Integer.to_string())

    # 1. Define Schema and Start Node
    schema = [{"a", 5}, "b", {"c", 7}]
    shard_config = [strategy: :one_per_key, schema: schema]
    opts = [node_id: node_id, transaction: [shards: shard_config]]

    enode = ENode.start_node(opts)
    assert %ENode{node_id: ^node_id} = enode

    # 2. Verify Shard Processes Exist (now safe to check)
    pid_shard_a = Registry.whereis(node_id, Shard, :a)
    pid_shard_b = Registry.whereis(node_id, Shard, :b)
    pid_shard_c = Registry.whereis(node_id, Shard, :c)

    assert is_pid(pid_shard_a), "Shard 'a' should be registered and alive."
    assert is_pid(pid_shard_b), "Shard 'b' should be registered and alive."
    assert is_pid(pid_shard_c), "Shard 'c' should be registered and alive."

    # 3. Verify Initial State within Shards (don't use stateful Shard.read)
    state_a = Shard.debug_get_state(pid_shard_a)
    state_b = Shard.debug_get_state(pid_shard_b)
    state_c = Shard.debug_get_state(pid_shard_c)

    # Check initial value at height 0
    assert state_a.kv["a"][0].value == 5, "Shard 'a' initial value mismatch"
    assert state_b.kv == %{}, "Shard 'b' should have an empty initial kv map"
    assert state_c.kv["c"][0].value == 7, "Shard 'c' initial value mismatch"

    # 4. Query the Mnesia table for key -> shard_label mapping
    table_name = Tables.table_shard_key_map(node_id)

    read_tx = fn key ->
      :mnesia.transaction(fn ->
        :mnesia.read({table_name, key})
      end)
    end

    assert read_tx.("a") == {:atomic, [{table_name, "a", :a}]},
           "Mnesia lookup for 'a' failed"

    assert read_tx.("b") == {:atomic, [{table_name, "b", :b}]},
           "Mnesia lookup for 'b' failed"

    assert read_tx.("c") == {:atomic, [{table_name, "c", :c}]},
           "Mnesia lookup for 'c' failed"

    assert read_tx.("d") == {:atomic, []},
           "Mnesia lookup for unknown key 'd' should return empty list"

    enode
  end

  @doc """
  I test dynamically adding new shards using `ShardSupervisor.start_shard/3`
  after initial setup is complete.

  I reuse the node started by `shard_supervisor_startup_and_routing/0`.
  """
  @spec dynamic_shard_start_test() :: ENode.t()
  def dynamic_shard_start_test() do
    # 1. Get a node with initial shards (a, b, c)
    # shard_supervisor_startup_and_routing creates a unique node
    enode = shard_supervisor_startup_and_routing()
    # Use the unique node_id from the setup
    node_id = enode.node_id

    # 2. Dynamically start a new shard 'd' with an initial value
    assert {:ok, pid_shard_d} = Supervisor.start_shard(node_id, "d", 10)
    assert is_pid(pid_shard_d)
    assert pid_shard_d == Registry.whereis(node_id, Shard, :d)

    # 3. Dynamically start a new shard 'e' without an initial value
    assert {:ok, pid_shard_e} = Supervisor.start_shard(node_id, "e")
    assert is_pid(pid_shard_e)
    assert pid_shard_e == Registry.whereis(node_id, Shard, :e)

    # 4. Try to start an existing shard 'a' again (should fail)
    # DynamicSupervisor returns {:error, {:already_started, pid}} if the child
    # process with the same registered name is already running.
    assert {:error, {:already_started, existing_pid_a}} =
             Supervisor.start_shard(node_id, "a", 99)

    assert is_pid(existing_pid_a)
    assert existing_pid_a == Registry.whereis(node_id, Shard, :a)

    # 5. Verify initial state within new shards 'd' and 'e'
    state_d = Shard.debug_get_state(pid_shard_d)
    state_e = Shard.debug_get_state(pid_shard_e)

    assert state_d.kv["d"][0].value == 10, "Shard 'd' initial value mismatch"
    assert state_e.kv == %{}, "Shard 'e' should have an empty initial kv map"

    # 6. Query the Mnesia table for new key mappings
    table_name = Tables.table_shard_key_map(node_id)

    read_tx = fn key ->
      :mnesia.transaction(fn ->
        :mnesia.read({table_name, key})
      end)
    end

    # Verify 'a' IS in the map (from initial setup of this unique node)
    assert read_tx.("a") == {:atomic, [{table_name, "a", :a}]},
           "Mnesia lookup for 'a' failed"

    assert read_tx.("d") == {:atomic, [{table_name, "d", :d}]},
           "Mnesia lookup for 'd' failed"

    assert read_tx.("e") == {:atomic, [{table_name, "e", :e}]},
           "Mnesia lookup for 'e' failed"

    enode
  end
end
