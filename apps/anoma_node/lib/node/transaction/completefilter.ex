defmodule Anoma.Node.Transaction.Backends.CompleteFilter do
  @moduledoc """
  I am a filter for backend result messages.
  """

  alias Anoma.Node.Transaction.Backends

  use TypedStruct
  use EventBroker.Filter

  typedstruct do
  end

  def filter(
        %EventBroker.Event{body: %Backends.CompleteEvent{}},
        _filter_params
      ) do
    true
  end

  def filter(_msg, _filter_params) do
    false
  end
end
