defmodule Anoma.Node.Transaction.Ordering.TxIdFilter do
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

    field(:tx_id, binary())
  end

  def filter(msg, filter_params) do
    msg.body.tx_id == filter_params.tx_id
  end
end
