defmodule Anoma.Node.Event do
  @moduledoc """
  I am an Node Event module for Anoma Node.
  I provide a wrapper around all Anoma Events.
  """

  require EventBroker.Event
  use TypedStruct

  typedstruct enforce: true do
    @typedoc """
    I am the Event type for the Node events.
    My fields determine the overall structure of the Node messages.
    ### Fields
    - `:node_id` - The ID of the Anoma Node sending an event. Nil in case
                   of a non-Anoma event.
    - `:body` - A body of the event.
    """

    field(:node_id, any())
    field(:body, term())
  end

  defmacro new_with_body(node_id, body) do
    quote do
      %EventBroker.Event{
        source_module: __MODULE__,
        body: %Anoma.Node.Event{
          node_id: unquote(node_id),
          body: unquote(body)
        }
      }
    end
  end
end
