defmodule Anoma.Node.Mempool.Ordering.IdFilter do
  @moduledoc """
  I am a filter for matching ordered transaction ids.
  """

  use TypedStruct
  use EventBroker.Filter

  typedstruct enforce: true do
    @typedoc """
    I store a value for filtering.

    ### Fields

    - `:id` - A binary
    """

    field(:id, binary())
  end

  def filter(msg, filter_params) do
    msg.body.id == filter_params.id
  end
end
