defmodule Anoma.Node.Logging.LoggingFilter do
  @moduledoc """
  I am a filter for the logging engine.

  This includes both logs and events.
  """

  alias Anoma.Node.Transaction.Mempool

  use TypedStruct
  use EventBroker.Filter

  typedstruct do
  end

  def filter(
        %EventBroker.Event{body: %Anoma.Node.Logging.LoggingEvent{}},
        _filter_params
      ) do
    true
  end

  def filter(%EventBroker.Event{body: %Mempool.TxEvent{}}, _filter_params) do
    true
  end

  def filter(
        %EventBroker.Event{body: %Mempool.ConsensusEvent{}},
        _filter_params
      ) do
    true
  end

  def filter(%EventBroker.Event{body: %Mempool.BlockEvent{}}, _filter_params) do
    true
  end

  def filter(_msg, _filter_params) do
    false
  end
end
