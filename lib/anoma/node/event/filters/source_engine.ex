defmodule Anoma.Node.Event.Filters.SourceEngine do
  @moduledoc """
  I filter Anoma node events by source engine.
  """

  use EventBroker.Filter
  use TypedStruct
  alias __MODULE__

  typedstruct enforce: true do
    field(:source_engine, Anoma.Node.Router.Addr.t())
  end

  def filter(
        %EventBroker.Event{
          body: %Anoma.Node.Event{source_engine: msg_source_engine}
        },
        %SourceEngine{source_engine: filter_source_engine}
      ) do
    msg_source_engine == filter_source_engine
  end

  def filter(_msg, _params) do
    false
  end
end
