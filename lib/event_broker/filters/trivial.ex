defmodule EventBroker.Filters.Trivial do
  @moduledoc """
  the trivial filter
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
