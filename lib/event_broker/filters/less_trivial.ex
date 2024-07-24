defmodule EventBroker.Filters.LessTrivial do
  @moduledoc false

  use TypedStruct

  typedstruct enforce: true do
    field(:value, bool())
  end

  @spec filter(EventBroker.Event.t(), t()) :: bool()
  def filter(_, filter_params) do
    filter_params.value
  end
end
