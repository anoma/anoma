defmodule EventBroker.Filters do
  use EventBroker.Filter

  @doc """
  I am the trivial filter. I always return true.
  """
  defilter Trivial, [] do
    _ -> true
  end

  @doc """
  I represent a filter which filters a message based on a value.

  I either always return true or always return false.

  I have the following parameters.

  - `:value` - `any()`
  """
  defilter LessTrivial, value: String.t() do
    _ -> value
  end

  @doc """
  I filter an event based on its source module.

  I have the following parameters.

   - `:module` - `atom()`
  """
  defilter SourceModule, module: atom() do
    %EventBroker.Event{source_module: event_module} ->
      event_module == module

    _ ->
      false
  end
end
