defmodule EventBroker.EventStruct do
  @moduledoc """
  I contain the `eventstruct` macro, which combines `typedstruct` and `deffilter` 
  functionality to create both event structs and their associated filters in one go.
  
  This macro reduces code duplication by automatically generating the filter
  that matches the event struct being defined.
  """

  defmacro __using__(_opts) do
    quote do
      import TypedStruct, only: [typedstruct: 1, typedstruct: 2]
      import EventBroker.DefFilter, only: [deffilter: 2]
      import EventBroker.EventStruct, only: [eventstruct: 2, eventstruct: 3]
    end
  end

  @doc """
  I am the `eventstruct` macro. I define a typed struct for an event and automatically
  create an associated filter that matches that event.

  ### Parameters
    - `event_module` - The name of the event module.
    - `opts` - Keyword options to configure the struct and filter

  ### Options
      * `:enforce` - When set to true (default), fields in the struct are enforced.
      * `:filter` - The name of the filter module to be created. If not specified,
        it will be automatically generated as `#{EventModule}Filter`.

  ### Example
    ```elixir
    eventstruct TxEvent do
      field(:id, binary())
      field(:tx, Mempool.Tx.t())
    end
    ```

    This will create:
    - A `TxEvent` struct with the specified fields
    - A `TxEventFilter` that matches events containing `TxEvent` structs
  """
  defmacro eventstruct(event_module, opts \\ [], do: block) do
    enforce = Keyword.get(opts, :enforce, true)
    filter_module = Keyword.get(opts, :filter, nil)

    full_event_module = prepare_module_name(event_module, __CALLER__)
    full_filter_module = prepare_filter_module_name(event_module, filter_module, __CALLER__)

    quote do
      unquote(define_struct(full_event_module, enforce, block))
      unquote(define_auto_filter(full_event_module, full_filter_module))
    end
  end

  defp prepare_module_name(module, caller) do
    Module.concat(caller.module, Macro.expand(module, caller))
  end

  defp prepare_filter_module_name(event_module, nil, caller) do
    # Auto-generate filter name by appending "Filter" to event module name
    event_name = Macro.expand(event_module, caller) |> Atom.to_string()
    filter_name = event_name <> "Filter" |> String.to_atom()
    Module.concat(caller.module, filter_name)
  end

  defp prepare_filter_module_name(_event_module, filter_module, caller) do
    Module.concat(caller.module, Macro.expand(filter_module, caller))
  end

  defp define_struct(full_event_module, enforce, block) do
    quote do
      typedstruct enforce: unquote(enforce),
                  module: unquote(full_event_module) do
        unquote(block)
      end
    end
  end

  defp define_auto_filter(event_module, filter_module) do
    quote do
      deffilter unquote(filter_module) do
        %EventBroker.Event{body: %Anoma.Node.Event{body: %unquote(event_module){}}} ->
          true

        _ ->
          false
      end
    end
  end
end
