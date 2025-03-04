defmodule Anoma.Node.Examples.EEvent.EDefEvent do
  alias Anoma.Node
  alias Anoma.Node.Examples.ENode
  alias Anoma.Node.Config

  require Node.Event
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
        defmodule TestEventModule do
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
        assert %TestEventModule.SimpleEvent{
                 id: 123,
                 result: {:ok, "success"}
               } ==
                 %TestEventModule.SimpleEvent{
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
        defmodule EnforcedEventModule do
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
          %EnforcedEventModule.EnforcedEvent{id: 1}
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
        defmodule FilterEventModule do
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
        assert Code.ensure_loaded?(FilterEventModule.CustomFilter)

        assert function_exported?(
                 FilterEventModule.CustomFilter,
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
        defmodule NoFilterEventModule do
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
        refute Code.ensure_loaded?(NoFilterEventModule.NoFilterEventFilter)
      end

    Code.eval_quoted(quoted_assertion)
  end

  defmodule EventExample do
    @moduledoc """
    I define custom events `FooEvent` and `BarEvent` with filters.
    """

    use Anoma.Node.Event.DefEvent

    defevent FooEvent, filter: FooFilter do
      @typedoc """
      I represent an event of type Foo
      """

      field(:id, integer())
    end

    defevent BarEvent, filter: BarFilter do
      @typedoc """
      I represent an event of type Bar
      """

      field(:id, integer())
    end
  end

  @doc """
  I demonstrate filtered event subscription using `FooEvent` and `BarEvent`.
  """
  @spec filtered_event_subscription(String.t()) :: String.t()
  def filtered_event_subscription(node_id \\ Node.example_random_id()) do
    ENode.start_noded(node_config: Config.node(node_id))

    filter_foo = %EventExample.FooFilter{}
    filter_bar = %EventExample.BarFilter{}

    EventBroker.subscribe_me([filter_foo])

    event_foo =
      Node.Event.new_with_body(node_id, %EventExample.FooEvent{
        id: 100
      })

    event_bar =
      Node.Event.new_with_body(node_id, %EventExample.BarEvent{
        id: 200
      })

    EventBroker.event(event_foo)
    EventBroker.event(event_bar)

    assert_receive %EventBroker.Event{
      body: %Node.Event{
        body: %EventExample.FooEvent{
          id: 100
        },
        node_id: ^node_id
      },
      source_module: __MODULE__
    }

    refute_receive %EventBroker.Event{
      body: %Node.Event{
        body: %EventExample.BarEvent{}
      }
    }

    EventBroker.subscribe_me([filter_bar])

    EventBroker.event(event_foo)
    EventBroker.event(event_bar)

    assert_receive %EventBroker.Event{
      body: %Node.Event{
        body: %EventExample.FooEvent{
          id: 100
        },
        node_id: ^node_id
      },
      source_module: __MODULE__
    }

    assert_receive %EventBroker.Event{
      body: %Node.Event{
        body: %EventExample.BarEvent{
          id: 200
        },
        node_id: ^node_id
      }
    }

    EventBroker.unsubscribe_me([filter_foo])
    EventBroker.unsubscribe_me([filter_bar])

    node_id
  end
end
