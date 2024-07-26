defmodule EventBroker.Filter do
  @moduledoc """
  A behaviour for filters.
  """

  @callback filter(EventBroker.Event.t(), struct()) :: bool()

  defmacro __using__(_opts) do
    quote do
      @behaviour EventBroker.Filter
    end
  end
end
