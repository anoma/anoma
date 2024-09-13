defmodule EventBroker.Filters.Trivial do
  @moduledoc """
  I am the trivial filter. I always return true.
  """

  use TypedStruct
  use EventBroker.Filter

  typedstruct enforce: true do
  end

  @spec filter(EventBroker.Event.t(), t()) :: bool()
  def filter(_msg, _filter_params) do
    true
  end
end
