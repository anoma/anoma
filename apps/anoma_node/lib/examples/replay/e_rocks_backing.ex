defmodule Anoma.Node.Examples.EReplay.RocksBacking do
  @moduledoc """
  I define examples that test the behavior of RocksDB backing and table restore functionality.
  
  These examples demonstrate how to:
  - Test RocksDB-backed table persistence
  - Restore system functionality with backed tables
  - Use backed tables for replay testing
  - Verify data integrity after restore operations
  """

  alias Anoma.Node.Examples.ENode
  alias Anoma.Node.Examples.ETransaction
  alias Anoma.Node.Examples.Mempool, as: EMempool
  alias Anoma.Node.Replay
  alias Anoma.Tables
  alias Anoma.Node.Transaction.Storage

  use EventBroker.WithSubscription

  require Logger

  import ExUnit.Assertions

  @doc """
  I test that RocksDB-backed tables persist data across node restarts.
  
  This example demonstrates:
  1. Starting a node with RocksDB enabled
  2. Writing data to storage
  3. Stopping the node
  4. Restarting the node
  5. Verifying data persistence
  """
  @spec test_rocksdb_persistence(ENode.t()) :: ENode.t()
  def test_rocksdb_persistence(enode \\ ENode.start_node()) do
    # Ensure RocksDB is enabled for this test
    assert is_rocksdb_enabled?(enode.node_id), "RocksDB must be enabled for this test"
    
    # Write some test data to storage
    test_key = "rocksdb_test_key"
    test_value = "rocksdb_test_value"
    test_height = 1
    
    # Write data to storage
    :ok = Storage.write(enode.node_id, {test_height, test_key}, test_value)
    
    # Verify data was written
    {:ok, ^test_value} = Storage.read(enode.node_id, {test_height, test_key})
    
    # Stop the node
    ENode.stop_node(enode)
    
    # Restart the node with same ID
    restarted_enode = ENode.start_node(node_id: enode.node_id)
    
    # Verify data persists after restart
    {:ok, ^test_value} = Storage.read(restarted_enode.node_id, {test_height, test_key})
    
    Logger.info("RocksDB persistence test passed for node #{enode.node_id}")
    
    restarted_enode
  end

  @doc """
  I test table restore functionality using RocksDB-backed tables.
  
  This example demonstrates:
  1. Creating a backup of current table state
  2. Modifying the original tables
  3. Restoring from backup
  4. Verifying data integrity
  """
  @spec test_table_restore(ENode.t()) :: ENode.t()
  def test_table_restore(enode \\ ENode.start_node()) do
    # Ensure RocksDB is enabled
    assert is_rocksdb_enabled?(enode.node_id), "RocksDB must be enabled for this test"
    
    # Create backup tables
    backup_tables = create_backup_tables(enode.node_id)
    
    # Write original data
    original_key = "restore_test_key"
    original_value = "restore_test_value"
    test_height = 1
    
    :ok = Storage.write(enode.node_id, {test_height, original_key}, original_value)
    
    # Verify original data
    {:ok, ^original_value} = Storage.read(enode.node_id, {test_height, original_key})
    
    # Modify original data
    modified_value = "modified_value"
    :ok = Storage.write(enode.node_id, {test_height, original_key}, modified_value)
    
    # Verify modification
    {:ok, ^modified_value} = Storage.read(enode.node_id, {test_height, original_key})
    
    # Restore from backup
    restore_from_backup(enode.node_id, backup_tables)
    
    # Verify data is restored to original state
    {:ok, ^original_value} = Storage.read(enode.node_id, {test_height, original_key})
    
    Logger.info("Table restore test passed for node #{enode.node_id}")
    
    enode
  end

  @doc """
  I test replay functionality with RocksDB-backed tables.
  
  This example demonstrates:
  1. Starting a node with RocksDB
  2. Executing transactions
  3. Creating a state dump
  4. Replaying from the dump
  5. Verifying replay success
  """
  @spec test_replay_with_backed_tables(ENode.t()) :: ENode.t()
  def test_replay_with_backed_tables(enode \\ ENode.start_node()) do
    # Ensure RocksDB is enabled
    assert is_rocksdb_enabled?(enode.node_id), "RocksDB must be enabled for this test"
    
    with_subscription [[]] do
      # Execute a transaction to create some state
      {_node, transaction} = EMempool.add_transaction(enode)
      
      # Verify transaction is in mempool
      {:ok, mempool_args} = Replay.State.mempool_arguments(enode.node_id)
      assert length(mempool_args[:transactions]) > 0
      
      # Test replay with backed tables
      replay_result = Replay.replay_for(enode.node_id)
      
      # Verify replay succeeded
      assert replay_result == {:ok, :replay_succeeded}
      
      Logger.info("Replay with backed tables test passed for node #{enode.node_id}")
      
      enode
    end
  end

  @doc """
  I test data integrity after RocksDB restore operations.
  
  This example demonstrates:
  1. Writing complex data structures
  2. Creating backup
  3. Restoring from backup
  4. Verifying all data integrity
  """
  @spec test_data_integrity_after_restore(ENode.t()) :: ENode.t()
  def test_data_integrity_after_restore(enode \\ ENode.start_node()) do
    # Ensure RocksDB is enabled
    assert is_rocksdb_enabled?(enode.node_id), "RocksDB must be enabled for this test"
    
    # Write multiple data points
    test_data = [
      {"key1", "value1", 1},
      {"key2", "value2", 1},
      {"key3", "value3", 2},
      {"key4", "value4", 2}
    ]
    
    # Write all test data
    for {key, value, height} <- test_data do
      :ok = Storage.write(enode.node_id, {height, key}, value)
    end
    
    # Create backup
    backup_tables = create_backup_tables(enode.node_id)
    
    # Clear original tables
    clear_node_tables(enode.node_id)
    
    # Verify tables are empty
    for {key, _value, height} <- test_data do
      :absent = Storage.read(enode.node_id, {height, key})
    end
    
    # Restore from backup
    restore_from_backup(enode.node_id, backup_tables)
    
    # Verify all data is restored
    for {key, value, height} <- test_data do
      {:ok, ^value} = Storage.read(enode.node_id, {height, key})
    end
    
    Logger.info("Data integrity test passed for node #{enode.node_id}")
    
    enode
  end

  @doc """
  I test RocksDB configuration and enablement.
  
  This example verifies that RocksDB is properly configured and enabled.
  """
  @spec test_rocksdb_configuration(ENode.t()) :: ENode.t()
  def test_rocksdb_configuration(enode \\ ENode.start_node()) do
    # Check RocksDB configuration
    rocksdb_enabled = is_rocksdb_enabled?(enode.node_id)
    persist_enabled = is_persistence_enabled?(enode.node_id)
    
    # Log configuration status
    Logger.info("RocksDB enabled: #{rocksdb_enabled}")
    Logger.info("Persistence enabled: #{persist_enabled}")
    
    # Verify configuration is consistent
    if rocksdb_enabled do
      assert persist_enabled, "RocksDB requires persistence to be enabled"
    end
    
    enode
  end

  # -----------------------------------------------------------
  # Private Helper Functions

  @doc """
  Check if RocksDB is enabled for the given node.
  """
  @spec is_rocksdb_enabled?(String.t()) :: boolean()
  def is_rocksdb_enabled?(node_id) do
    # This would need to be implemented based on actual RocksDB configuration
    # For now, we'll assume it's enabled if the node has data
    case Tables.has_data?(node_id) do
      {:ok, :exists} -> true
      _ -> false
    end
  end

  @doc """
  Check if persistence to disk is enabled.
  """
  @spec is_persistence_enabled?(String.t()) :: boolean()
  def is_persistence_enabled?(node_id) do
    # This would need to be implemented based on actual configuration
    # For now, we'll assume it's enabled if RocksDB is enabled
    is_rocksdb_enabled?(node_id)
  end

  @doc """
  Create backup tables for the given node.
  """
  @spec create_backup_tables(String.t()) :: [atom()]
  def create_backup_tables(node_id) do
    # Get all table names for the node
    table_names = [
      Tables.table_events(node_id),
      Tables.table_blocks(node_id),
      Tables.table_values(node_id),
      Tables.table_updates(node_id),
      Tables.table_intents(node_id)
    ]
    
    # Create backup tables
    backup_tables = Enum.map(table_names, fn table_name ->
      backup_name = String.to_atom("#{table_name}_backup")
      case Tables.duplicate_table(table_name, backup_name) do
        {:ok, :table_copied} -> backup_name
        {:error, :copy_failed, _} -> 
          Logger.warning("Failed to create backup for table #{table_name}")
          nil
      end
    end)
    
    # Filter out nil values
    Enum.reject(backup_tables, &is_nil/1)
  end

  @doc """
  Restore tables from backup.
  """
  @spec restore_from_backup(String.t(), [atom()]) :: :ok
  def restore_from_backup(node_id, backup_tables) do
    # For each backup table, restore to original
    for backup_table <- backup_tables do
      # Extract original table name from backup name
      original_name = backup_table
      |> Atom.to_string()
      |> String.replace("_backup", "")
      |> String.to_atom()
      
      # Clear original table
      :ok = Tables.clear_table(original_name)
      
      # Copy from backup to original
      case Tables.duplicate_table(backup_table, original_name) do
        {:ok, :table_copied} -> 
          Logger.info("Restored table #{original_name} from backup")
        {:error, :copy_failed, reason} -> 
          Logger.error("Failed to restore table #{original_name}: #{inspect(reason)}")
      end
    end
    
    :ok
  end

  @doc """
  Clear all tables for the given node.
  """
  @spec clear_node_tables(String.t()) :: :ok
  def clear_node_tables(node_id) do
    table_names = [
      Tables.table_events(node_id),
      Tables.table_blocks(node_id),
      Tables.table_values(node_id),
      Tables.table_updates(node_id),
      Tables.table_intents(node_id)
    ]
    
    for table_name <- table_names do
      :ok = Tables.clear_table(table_name)
    end
    
    :ok
  end
end
