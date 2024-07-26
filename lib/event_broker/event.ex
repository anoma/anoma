defmodule EventBroker.Event do
  @moduledoc """
  an event that goes through the event broker
  """

  use TypedStruct

  typedstruct enforce: true do
    field(:source_module, module())
    field(:body, term())
  end
end
