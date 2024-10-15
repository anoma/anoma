defmodule Anoma.Node.Transaction.Backends.ForMempoolFilter do
  @moduledoc """
  Important messages for mempool.
  """

  alias Anoma.Node.Transaction.{Backends, Executor}

  use TypedStruct
  use EventBroker.Filter

  typedstruct do
  end

  def filter(
        %EventBroker.Event{body: %Backends.ResultEvent{}},
        _filter_params
      ) do
    true
  end

  def filter(
        %EventBroker.Event{body: %Executor.ExecutionEvent{}},
        _filter_params
      ) do
    true
  end

  def filter(_msg, _filter_params) do
    false
  end
end
