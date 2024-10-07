defmodule Anoma.Node.Utility.Consensus.BlockFilter do
  @moduledoc """
  I am a filter for Block events.
  """

  alias Anoma.Node.Transaction.Mempool

  use TypedStruct
  use EventBroker.Filter

  typedstruct do
  end

  def filter(%EventBroker.Event{body: %Mempool.BlockEvent{}}, _filter_params) do
    true
  end

  def filter(_msg, _filter_params) do
    false
  end
end
