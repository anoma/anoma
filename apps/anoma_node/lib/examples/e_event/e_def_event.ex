defmodule Anoma.Node.Examples.EEvent.EDefEvent do
  require ExUnit.Assertions
  import ExUnit.Assertions

  @doc """
  I create a typed struct with specified fields.

  I assert that the struct can be created with the expected values.
  """
  @spec create_struct_with_fields() :: {term(), Code.binding()}
  def create_struct_with_fields() do
    quoted_module =
      quote do
        defmodule DynamicTestEventModule do
          use Anoma.Node.Event.DefEvent

          defevent SimpleEvent do
            field(:id, integer())
            field(:result, {:ok, any()} | :error)
          end
        end
      end

    Code.eval_quoted(quoted_module)

    quoted_assertion =
      quote do
        assert %DynamicTestEventModule.SimpleEvent{
                 id: 123,
                 result: {:ok, "success"}
               } ==
                 %DynamicTestEventModule.SimpleEvent{
                   id: 123,
                   result: {:ok, "success"}
                 }
      end

    Code.eval_quoted(quoted_assertion)
  end

  @doc """
  I enforce required fields when the `enforce` option is true.

  I assert that an error is raised when required fields are missing.
  """
  @spec enforce_required_fields() :: any()
  def enforce_required_fields() do
    quoted_module =
      quote do
        defmodule DynamicEnforcedEventModule do
          use Anoma.Node.Event.DefEvent

          defevent EnforcedEvent, enforce: true do
            field(:id, integer())
            field(:status, String.t())
          end
        end
      end

    Code.eval_quoted(quoted_module)

    assert_raise ArgumentError, fn ->
      Code.eval_quoted(
        quote do
          # Missing required :status field
          %DynamicEnforcedEventModule.EnforcedEvent{id: 1}
        end
      )
    end
  end

  @doc """
  I create a filter module when the `filter` option is provided.

  I assert that the filter module is created with the expected functions.
  """
  @spec create_filter_module() :: {term(), Code.binding()}
  def create_filter_module() do
    quoted_module =
      quote do
        defmodule DynamicFilterEventModule do
          use Anoma.Node.Event.DefEvent

          defevent FilteredEvent, filter: CustomFilter do
            field(:id, integer())
            field(:status, String.t())
          end
        end
      end

    Code.eval_quoted(quoted_module)

    quoted_assertion =
      quote do
        assert Code.ensure_loaded?(DynamicFilterEventModule.CustomFilter)

        assert function_exported?(
                 DynamicFilterEventModule.CustomFilter,
                 :filter,
                 2
               )
      end

    Code.eval_quoted(quoted_assertion)
  end

  @doc """
  I do not create a filter module when the `filter` option is nil.

  I assert that no filter module is created.
  """
  @spec no_filter_module() :: {term(), Code.binding()}
  def no_filter_module() do
    quoted_module =
      quote do
        defmodule DynamicNoFilterEventModule do
          use Anoma.Node.Event.DefEvent

          defevent NoFilterEvent, filter: nil do
            field(:id, integer())
            field(:result, {:ok, any()} | :error)
          end
        end
      end

    Code.eval_quoted(quoted_module)

    quoted_assertion =
      quote do
        refute Code.ensure_loaded?(
                 DynamicNoFilterEventModule.NoFilterEventFilter
               )
      end

    Code.eval_quoted(quoted_assertion)
  end
end
