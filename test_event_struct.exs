#!/usr/bin/env elixir

# Simple test script for the eventstruct macro
# Run with: elixir test_event_struct.exs

# Load the EventBroker.EventStruct module
Code.require_file("apps/event_broker/lib/event_broker/event_struct.ex")

# Test module using the eventstruct macro
defmodule TestEventStruct do
  use EventBroker.EventStruct

  # Test basic usage
  eventstruct TestEvent do
    field(:id, binary())
    field(:data, String.t())
  end

  # Test with custom filter name
  eventstruct CustomEvent, filter: MyCustomFilter do
    field(:value, integer())
  end
end

# Print the generated modules
IO.puts("Generated modules:")
IO.puts("TestEvent: #{inspect(TestEventStruct.TestEvent)}")
IO.puts("TestEventFilter: #{inspect(TestEventStruct.TestEventFilter)}")
IO.puts("CustomEvent: #{inspect(TestEventStruct.CustomEvent)}")
IO.puts("MyCustomFilter: #{inspect(TestEventStruct.MyCustomFilter)}")

# Test creating instances
test_event = %TestEventStruct.TestEvent{id: "123", data: "test"}
custom_event = %TestEventStruct.CustomEvent{value: 42}

IO.puts("\nCreated instances:")
IO.puts("TestEvent: #{inspect(test_event)}")
IO.puts("CustomEvent: #{inspect(custom_event)}")

IO.puts("\nTest completed successfully!")
