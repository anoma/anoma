# RocksDB Backing Implementation

## Overview

This implementation addresses issue #1609: "Test Rocks-Backing of Tables". The goal is to add examples for restoring system functionality with backed tables and using them for replay testing.

## Problem

Currently, Anoma has RocksDB backing functionality but lacks comprehensive examples and tests for:
- Testing RocksDB-backed table persistence
- Restoring system functionality with backed tables
- Using backed tables for replay testing
- Verifying data integrity after restore operations

## Solution

The new implementation provides comprehensive examples and tests that demonstrate:

1. **RocksDB Persistence Testing**: Verify data persists across node restarts
2. **Table Backup and Restore**: Create backup tables and restore from them
3. **Replay Testing with Backed Tables**: Test replay functionality using RocksDB-backed storage
4. **Data Integrity Verification**: Ensure data integrity after restore operations

## Implementation Details

### Files Created

1. **`apps/anoma_node/lib/examples/replay/e_rocks_backing.ex`** - Main examples module
2. **`apps/anoma_node/test/rocks_backing_test.exs`** - Basic test file
3. **`apps/anoma_node/test/rocks_backing_integration_test.exs`** - Integration tests
4. **`ROCKS_BACKING_IMPLEMENTATION.md`** - This documentation

### Key Features

#### 1. RocksDB Persistence Testing
```elixir
test_rocksdb_persistence(enode)
```
- Tests data persistence across node restarts
- Verifies RocksDB backing is working correctly
- Ensures data survives node shutdown/restart cycles

#### 2. Table Backup and Restore
```elixir
test_table_restore(enode)
```
- Creates backup tables using `Tables.duplicate_table/2`
- Modifies original data
- Restores from backup
- Verifies data integrity

#### 3. Replay Testing with Backed Tables
```elixir
test_replay_with_backed_tables(enode)
```
- Executes transactions to create state
- Tests replay mechanism with RocksDB-backed storage
- Verifies replay success

#### 4. Data Integrity Verification
```elixir
test_data_integrity_after_restore(enode)
```
- Writes complex data structures
- Creates comprehensive backups
- Restores from backup
- Verifies all data integrity

#### 5. Configuration Verification
```elixir
test_rocksdb_configuration(enode)
```
- Checks RocksDB enablement status
- Verifies persistence configuration
- Ensures configuration consistency

### Helper Functions

#### Backup Management
- `create_backup_tables/1`: Creates backup copies of all node tables
- `restore_from_backup/2`: Restores tables from backup
- `clear_node_tables/1`: Clears all tables for a node

#### Configuration Checks
- `is_rocksdb_enabled?/1`: Checks if RocksDB is enabled
- `is_persistence_enabled?/1`: Checks if persistence is enabled

## Usage Examples

### Basic RocksDB Testing
```elixir
# Start a node
enode = ENode.start_node()

# Test RocksDB persistence
RocksBacking.test_rocksdb_persistence(enode)

# Test table restore functionality
RocksBacking.test_table_restore(enode)

# Test replay with backed tables
RocksBacking.test_replay_with_backed_tables(enode)
```

### Advanced Testing
```elixir
# Test data integrity after restore
RocksBacking.test_data_integrity_after_restore(enode)

# Verify RocksDB configuration
RocksBacking.test_rocksdb_configuration(enode)

# Create and manage backup tables
backup_tables = RocksBacking.create_backup_tables(enode.node_id)
RocksBacking.restore_from_backup(enode.node_id, backup_tables)
```

## Testing

### Running Tests

#### Basic Tests
```bash
mix test apps/anoma_node/test/rocks_backing_test.exs
```

#### Integration Tests
```bash
mix test apps/anoma_node/test/rocks_backing_integration_test.exs
```

#### Specific Test Tags
```bash
# Run only RocksDB tests
mix test --only rocksdb

# Run only integration tests
mix test --only integration

# Run RocksDB integration tests
mix test --only rocksdb --only integration
```

### Test Categories

1. **Unit Tests** (`:unit`): Test individual functions
2. **Integration Tests** (`:integration`): Test complete workflows
3. **RocksDB Tests** (`:rocksdb`): Tests requiring RocksDB

## Configuration Requirements

### RocksDB Enablement
Tests require RocksDB to be enabled in the configuration:
```elixir
config :anoma_node, :mnesia,
  persist_to_disk: true,
  rocksdb: true
```

### Test Environment
- Tests automatically skip if RocksDB is not enabled
- Graceful degradation for non-RocksDB environments
- Comprehensive logging for debugging

## Benefits

1. **Comprehensive Testing**: Covers all aspects of RocksDB backing
2. **Real-world Scenarios**: Tests actual use cases developers will encounter
3. **Data Integrity**: Ensures backup/restore operations maintain data integrity
4. **Replay Testing**: Validates replay functionality with backed storage
5. **Documentation**: Provides clear examples for developers

## Future Enhancements

1. **Performance Testing**: Add benchmarks for backup/restore operations
2. **Stress Testing**: Test with large datasets and complex state
3. **Recovery Testing**: Test various failure scenarios and recovery
4. **Cross-node Testing**: Test backup/restore across multiple nodes

## Dependencies

- `Anoma.Tables`: For table operations and backup functionality
- `Anoma.Node.Transaction.Storage`: For storage operations
- `Anoma.Node.Replay`: For replay testing
- `EventBroker.WithSubscription`: For event subscription management

## Conclusion

This implementation provides comprehensive testing and examples for RocksDB backing functionality, addressing the requirements of issue #1609. It enables developers to:

- Test RocksDB persistence reliably
- Verify backup/restore operations
- Validate replay functionality with backed storage
- Ensure data integrity across operations

The implementation follows Anoma's coding standards and provides robust, well-documented examples that can be used for both testing and learning purposes.
