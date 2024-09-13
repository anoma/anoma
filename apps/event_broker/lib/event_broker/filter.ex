defmodule EventBroker.Filter do
  @moduledoc """
  I am a filter module for the Event Broker. I provide the callbacks for
  all the filters used in the PubSub system.
  """

  @callback filter(EventBroker.Event.t(), struct()) :: bool()

  defmacro __using__(_opts) do
    quote do
      @behaviour EventBroker.Filter
    end
  end
end
