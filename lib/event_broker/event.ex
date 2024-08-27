defmodule EventBroker.Event do
  @moduledoc """
  I am an Event module for the Event Broker.

  I provide the standard event format to be distributed for the PubSub
  system. Unless the message is in my format it does not get processed.
  """

  use TypedStruct

  typedstruct enforce: true do
    @typedoc """
    I am the Event type for the Event Broker.

    My fields determine the overall structure of the messages.

    ### Fields

    - `:source_module` - The module from which the message got sent.
    - `:body` - A body of the event.
    """

    field(:source_module, module())
    field(:body, term())
  end

  defmacro new_with_body(body) do
    quote do
      %EventBroker.Event{source_module: __MODULE__, body: unquote(body)}
    end
  end
end
