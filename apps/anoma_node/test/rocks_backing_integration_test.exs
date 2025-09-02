defmodule RocksBackingIntegrationTest do
  use ExUnit.Case, async: false

  alias Anoma.Node.Examples.ENode
  alias Anoma.Node.Examples.EReplay.RocksBacking
  alias Anoma.Tables
  alias Anoma.Node.Transaction.Storage

  require Logger

  @moduledoc """
  Integration tests for RocksDB backing functionality.
  
  These tests verify:
  - RocksDB persistence across node restarts
  - Table backup and restore operations
  - Data integrity after restore operations
  - Replay functionality with backed tables
  """

  setup do
    # Ensure clean state for each test
    :ok
  end

  @tag :rocksdb
  @tag :integration
  test "RocksDB persistence across node restarts" do
    # Start a fresh node
    enode = ENode.start_node()
    
    # Skip test if RocksDB is not enabled
    unless RocksBacking.is_rocksdb_enabled?(enode.node_id) do
      Logger.warning("Skipping RocksDB persistence test - RocksDB not enabled")
      :ok
    else
      # Test RocksDB persistence
      result = RocksBacking.test_rocksdb_persistence(enode)
      assert result.node_id == enode.node_id
      
      # Clean up
      ENode.stop_node(result)
    end
  end

  @tag :rocksdb
  @tag :integration
  test "Table restore functionality with RocksDB backing" do
    # Start a fresh node
    enode = ENode.start_node()
    
    # Skip test if RocksDB is not enabled
    unless RocksBacking.is_rocksdb_enabled?(enode.node_id) do
      Logger.warning("Skipping table restore test - RocksDB not enabled")
      :ok
    else
      # Test table restore functionality
      result = RocksBacking.test_table_restore(enode)
      assert result.node_id == enode.node_id
      
      # Clean up
      ENode.stop_node(result)
    end
  end

  @tag :rocksdb
  @tag :integration
  test "Replay with RocksDB-backed tables" do
    # Start a fresh node
    enode = ENode.start_node()
    
    # Skip test if RocksDB is not enabled
    unless RocksBacking.is_rocksdb_enabled?(enode.node_id) do
      Logger.warning("Skipping replay test - RocksDB not enabled")
      :ok
    else
      # Test replay with backed tables
      result = RocksBacking.test_replay_with_backed_tables(enode)
      assert result.node_id == enode.node_id
      
      # Clean up
      ENode.stop_node(result)
    end
  end

  @tag :rocksdb
  @tag :integration
  test "Data integrity after RocksDB restore operations" do
    # Start a fresh node
    enode = ENode.start_node()
    
    # Skip test if RocksDB is not enabled
    unless RocksBacking.is_rocksdb_enabled?(enode.node_id) do
      Logger.warning("Skipping data integrity test - RocksDB not enabled")
      :ok
    else
      # Test data integrity after restore
      result = RocksBacking.test_data_integrity_after_restore(enode)
      assert result.node_id == enode.node_id
      
      # Clean up
      ENode.stop_node(result)
    end
  end

  @tag :rocksdb
  @tag :integration
  test "RocksDB configuration verification" do
    # Start a fresh node
    enode = ENode.start_node()
    
    # Test RocksDB configuration
    result = RocksBacking.test_rocksdb_configuration(enode)
    assert result.node_id == enode.node_id
    
    # Clean up
    ENode.stop_node(result)
  end

  @tag :rocksdb
  @tag :unit
  test "Backup table creation and management" do
    # Start a fresh node
    enode = ENode.start_node()
    
    # Skip test if RocksDB is not enabled
    unless RocksBacking.is_rocksdb_enabled?(enode.node_id) do
      Logger.warning("Skipping backup table test - RocksDB not enabled")
      :ok
    else
      # Test backup table creation
      backup_tables = RocksBacking.create_backup_tables(enode.node_id)
      assert is_list(backup_tables)
      
      # Verify backup tables exist
      for backup_table <- backup_tables do
        assert Tables.table_exists?(backup_table)
      end
      
      # Clean up backup tables
      for backup_table <- backup_tables do
        :ok = Tables.clear_table(backup_table)
      end
      
      # Clean up
      ENode.stop_node(enode)
    end
  end

  @tag :rocksdb
  @tag :unit
  test "Storage operations with RocksDB backing" do
    # Start a fresh node
    enode = ENode.start_node()
    
    # Skip test if RocksDB is not enabled
    unless RocksBacking.is_rocksdb_enabled?(enode.node_id) do
      Logger.warning("Skipping storage operations test - RocksDB not enabled")
      :ok
    else
      # Test basic storage operations
      test_key = "integration_test_key"
      test_value = "integration_test_value"
      test_height = 1
      
      # Write data
      :ok = Storage.write(enode.node_id, {test_height, test_key}, test_value)
      
      # Read data
      {:ok, ^test_value} = Storage.read(enode.node_id, {test_height, test_key})
      
      # Clean up
      ENode.stop_node(enode)
    end
  end
end
