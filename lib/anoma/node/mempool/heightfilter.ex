defmodule Anoma.Node.Mempool.Storage.HeightFilter do
  @moduledoc """
  I am a filter which for height-matching.
  """

  use TypedStruct
  use EventBroker.Filter

  typedstruct enforce: true do
    @typedoc """
    I store a value for filtering.

    ### Fields

    - `:height` - An integer
    """

    field(:height, non_neg_integer())
  end

  def filter(msg, filter_params) do
    msg.height == filter_params.height
  end
end
