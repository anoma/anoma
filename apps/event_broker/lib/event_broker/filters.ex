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

  deffilter ManyFields,
    event: %EventBroker.Event{
      source_module: event_module,
      body: {event_foo, event_bar}
    },
    params: %ManyFields{
      module: params_module,
      foo: params_foo,
      bar: params_bar
    },
    fields: [
      field(:module, module()),
      field(:foo, term()),
      field(:bar, term())
    ] do
    event_module == params_module && event_foo == params_foo &&
      event_bar == params_bar
  end

  deffilter SourceModule,
    event: %EventBroker.Event{source_module: event_module},
    params: %SourceModule{module: params_module},
    fields: [field(:module, module())] do
    event_module == params_module
  end
end
