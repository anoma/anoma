defmodule Anoma.Node.Event do
  @moduledoc """
  I am an Anoma node specific event. I go inside the body field of an EventBroker.Event.
  """

  use TypedStruct

  typedstruct enforce: true do
    field(:source_engine, Anoma.Node.Router.Addr.t())
    field(:body, term())
  end
end
