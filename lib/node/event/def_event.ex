defmodule Anoma.Node.Event.DefEvent do
  @moduledoc """
  I contain the `defevent` macro, which facilitates the definition of typed
  event structs and optionally associated filters. Require or use me to use `defevent`.
  """

  defmacro __using__(_opts) do
    quote do
      import TypedStruct, only: [typedstruct: 1, typedstruct: 2]
      import EventBroker.DefFilter, only: [deffilter: 2]
      import Anoma.Node.Event.DefEvent, only: [defevent: 2, defevent: 3]
    end
  end

  @doc """
  I am the `defevent` macro. I define a typed struct for an event and, if specified,
  an associated filter.

  ### Parameters
    - `event_module` - The name of the event module.
    - `opts` - Keyword options to configure the struct and filter

  ### Options
      * `:enforce` - When set to true (default), fields in the struct are enforced.
      * `:filter` - The name of the filter module to be created, if desired. If
        `nil`, no filter is created.

  ### Example
    ```
  defevent CompleteEvent, filter: CompleteFilter do
    field(:tx_id, integer())
    field(:tx_result, {:ok, any()} | :error)
  end
  ```

  ```
  defevent CompleteEvent, enforce: true, filter: CompleteFilter do
    field(:tx_id, integer())
    field(:tx_result, {:ok, any()} | :error)
  end
  ```
  """
  defmacro defevent(event_module, opts \\ [], do: block) do
    enforce = Keyword.get(opts, :enforce, true)
    filter_module = Keyword.get(opts, :filter, nil)

    full_event_module = prepare_module_name(event_module, __CALLER__)
    full_filter_module = prepare_module_name(filter_module, __CALLER__)

    quote do
      unquote(define_struct(full_event_module, enforce, block))
      unquote(define_filter(full_event_module, full_filter_module))
    end
  end

  defp prepare_module_name(nil, _caller), do: nil

  defp prepare_module_name(module, caller) do
    Module.concat(caller.module, Macro.expand(module, caller))
  end

  defp define_struct(full_event_module, enforce, block) do
    quote do
      typedstruct enforce: unquote(enforce),
                  module: unquote(full_event_module) do
        unquote(block)
      end
    end
  end

  defp define_filter(_event_module, nil) do
    quote do
    end
  end

  defp define_filter(event_module, filter_module) do
    quote do
      deffilter unquote(filter_module) do
        %EventBroker.Event{
          body: %Anoma.Node.Event{
            body: %unquote(event_module){}
          }
        } ->
          true

        _ ->
          false
      end
    end
  end
end
