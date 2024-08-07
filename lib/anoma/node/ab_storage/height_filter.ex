defmodule Anoma.Node.AbStorage.HeightFilter do
  use EventBroker.Filter
  use TypedStruct

  typedstruct enforce: true do
    field(:height, integer())
  end

  def filter(
        %EventBroker.Event{
          body: %Anoma.Node.AbStorage.WriteEvent{height: event_height}
        },
        %__MODULE__{height: filter_height}
      ) do
    event_height == filter_height
  end

  def filter(_, _) do
    false
  end
end
