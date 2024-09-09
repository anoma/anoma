defmodule EventBroker.Filter do
  @moduledoc """
  I am a filter module for the Event Broker. I provide a DSL to create filters.

  A filter can be instantiated by creating a struct with the necessary fields.

  ### Example

  ```elixir
  defilter OnlyFoo, [module: atom()] do
    %Event{source_module: event_module} ->
      event_module == module
  end

  my_filter = %OnlyFoo.Filter{module: Foo}
  ```
  ### Public API

  I have the following public functionality:

  #### Macros

  - `defilter/1`
  """

  defmacro __using__(_) do
    quote do
      import EventBroker.Filter
    end
  end

  @doc """
  I am the macro to define an event broker filter.

  When I'm invoked, I generate a new module with the given name as a submodule of the
  module that calls me.

  ### Pattern-Matching Variations

  - `init(%Clock{})` - I initialize the Engine with the given state.

  - `init(args)` - I expect a keylist and check for the :start key then
                   launch the Clock with said setting.
  """
  defmacro defilter(module_name, fields, do: body) do
    # the fields that can be set in an instance of this filter
    # give any field without a type the any() type.
    module_fields =
      fields
      |> Enum.map(fn
        {field, type} ->
          quote do
            {unquote(field), unquote(type)}
          end

        field ->
          quote do: {unquote(field), any()}
      end)

    # the names of the fields that can be set in an instance of this macro
    module_fields_names = Keyword.keys(module_fields)

    # inject all fields in the scope of the body of the filter
    scoped_vars =
      module_fields
      |> Enum.map(fn {field, _} ->
        quote do
          var!(unquote(Macro.var(field, nil))) =
            Map.get(var!(filter_instance), unquote(field))

          # supress unused variable warnings
          _ = var!(unquote(Macro.var(field, nil)))
        end
      end)

    # get the name of the module where defilter was called.
    %{module: mod} = __CALLER__

    quote do
      defmodule unquote(module_name) do
        # inject the moduledoc, if it was defined.
        attr = Module.delete_attribute(unquote(mod), :doc)

        if attr do
          @moduledoc elem(attr, 1)
        else
          @moduledoc nil
        end

        # require all keys to be passed when instantiating this filter
        @enforce_keys unquote(module_fields_names)

        # the type of the filters
        @type t :: %__MODULE__{
                unquote_splicing(module_fields)
              }

        # the struct to instantiate this filter
        defstruct unquote(module_fields_names)

        # apply function to test an instance of this filter against an event
        @spec apply(__MODULE__.t(), EventBroker.Event.t()) :: bool()
        def apply(filter_instance, event) do
          var!(filter_instance) = filter_instance
          # avoid unused variable warnings by assigning to _
          _ = var!(filter_instance)
          unquote_splicing(scoped_vars)

          case event do
            unquote(body)
          end
        end
      end
    end
  end
end
