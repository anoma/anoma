defmodule EventBroker.Event do
  @moduledoc """
  an event that goes through the event broker
  """

  use TypedStruct

  typedstruct enforce: true do
    field(:source_module, module())
    field(:body, term())
  end

  defmacro new_with_body(body) do
    quote do
      %EventBroker.Event{source_module: __MODULE__, body: unquote(body)}
    end
  end
end
