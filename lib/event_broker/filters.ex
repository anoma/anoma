defmodule EventBroker.Filters do
  @moduledoc """
  I contain submodules for the default filters supported by the event broker.
  """

  use EventBroker.DefFilter

  deffilter Trivial do
    true
  end

  deffilter LessTrivial,
    params: %LessTrivial{value: value},
    fields: [field(:value, bool())] do
    value
  end

  deffilter SourceModule,
    event: %EventBroker.Event{source_module: event_module},
    params: %SourceModule{module: params_module},
    fields: [field(:module, module())] do
    event_module == params_module
  end
end
