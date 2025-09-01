# EventStruct Macro Implementation

## Overview

This implementation addresses issue #1490: "Macro For Defining Events and Filters". The goal is to create a macro that combines `typedstruct` and `deffilter` functionality to reduce code duplication.

## Problem

Currently, developers need to write both a `typedstruct` and a `deffilter` for each event:

```elixir
typedstruct module: TxEvent do
  field(:id, binary())
  field(:tx, Mempool.Tx.t())
end

deffilter TxFilter do
  %EventBroker.Event{body: %Event{body: %TxEvent{}}} ->
    true
  _ ->
    false
end
```

## Solution

The new `eventstruct` macro combines both operations:

```elixir
eventstruct TxEvent do
  field(:id, binary())
  field(:tx, Mempool.Tx.t())
end
```

This automatically generates:
1. A `TxEvent` struct with the specified fields
2. A `TxEventFilter` that matches events containing `TxEvent` structs

## Implementation Details

### Files Created/Modified

1. **`apps/event_broker/lib/event_broker/event_struct.ex`** - New macro module
2. **`apps/event_broker/lib/examples/e_event_struct.ex`** - Example usage
3. **`apps/event_broker/lib/event_broker.ex`** - Added example usage
4. **`test_event_struct.exs`** - Test script

### Macro Features

- **Auto-generated filter names**: If no filter name is specified, it appends "Filter" to the event name
- **Custom filter names**: Can specify a custom filter name with `filter: MyCustomFilter`
- **Enforce option**: Supports `enforce: true/false` like `typedstruct`
- **Full compatibility**: Works with existing `typedstruct` and `deffilter` macros

### Usage Examples

```elixir
# Basic usage
eventstruct TestEvent do
  field(:id, binary())
  field(:data, String.t())
end

# With custom filter name
eventstruct CustomEvent, filter: MyCustomFilter do
  field(:value, integer())
end

# With enforce: false
eventstruct OptionalEvent, enforce: false do
  field(:required, String.t())
  field(:optional, integer(), default: 0)
end
```

## Testing

To test the implementation:

1. Install Elixir: `winget install Elixir.Elixir`
2. Run the test script: `elixir test_event_struct.exs`
3. Or compile the project: `mix compile`

## Benefits

1. **Reduced code duplication**: One macro call instead of two
2. **Consistency**: Ensures struct and filter are always paired
3. **Maintainability**: Changes to event structure automatically update the filter
4. **Developer experience**: Simpler, more intuitive API

## Next Steps

1. Add comprehensive tests
2. Update existing code to use the new macro
3. Add documentation to the main README
4. Create migration guide for existing code
