defmodule EventBroker.Filters do
  @moduledoc """
  I contain submodules for the default filters supported by the event broker.
  """

  use EventBroker.DefFilter

  deffilter Trivial do
    _ -> true
  end

  deffilter LessTrivial, value: bool() do
    _ -> value
  end

  # complicated macro usage example
  deffilter ManyFields,
    params_module: module(),
    params_foo: term(),
    params_bar: term() do
    %EventBroker.Event{
      source_module: event_module,
      body: {event_foo, event_bar}
    } ->
      params_module == event_module && params_foo == event_foo &&
        params_bar == event_bar
  end

  deffilter SourceModule, module: module() do
    %EventBroker.Event{source_module: ^module} -> true
    _ -> false
  end
end
