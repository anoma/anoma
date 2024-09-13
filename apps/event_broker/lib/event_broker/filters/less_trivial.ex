defmodule EventBroker.Filters.LessTrivial do
  @moduledoc """
  I represent a filter which filters a message based on the value I store
  in my struct.

  I either always return true or always return false.
  """

  use TypedStruct
  use EventBroker.Filter

  typedstruct enforce: true do
    @typedoc """
    I store a value for filtering.

    ### Fields

    - `:value` - A boolean value.
    """

    field(:value, bool())
  end

  @spec filter(EventBroker.Event.t(), t()) :: bool()
  def filter(_, filter_params) do
    filter_params.value
  end
end
