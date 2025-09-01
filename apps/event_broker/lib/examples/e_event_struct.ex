defmodule Examples.EEventBroker.EEventStruct do
  @moduledoc """
  I define examples on how to use the eventstruct macro to create both event structs
  and their associated filters in one go.
  """

  use EventBroker.EventStruct

  # Example 1: Basic usage with auto-generated filter name
  eventstruct TestEvent do
    field(:id, binary())
    field(:data, String.t())
  end

  # Example 2: With custom filter name
  eventstruct CustomEvent, filter: MyCustomFilter do
    field(:value, integer())
    field(:status, :ok | :error)
  end

  # Example 3: With enforce: false
  eventstruct OptionalEvent, enforce: false do
    field(:required, String.t())
    field(:optional, integer(), default: 0)
  end

  # Example 4: Complex event with multiple fields
  eventstruct ComplexEvent do
    field(:transaction_id, binary())
    field(:amount, float())
    field(:currency, String.t())
    field(:timestamp, DateTime.t())
    field(:metadata, map(), default: %{})
  end
end
