defmodule Anoma.Controller.Event do
  @moduledoc """
  I am an Controller Event module for Anoma Controller.
  I provide a wrapper around all Anoma Events.
  """

  require EventBroker.Event

  use EventBroker.DefFilter
  use TypedStruct

  typedstruct enforce: true do
    @typedoc """
    I am the Event type for the Controller events.
    My fields determine the overall structure of the Controller messages.
    ### Fields
    - `:node_id` - The ID of the Anoma Controller sending an event. Nil in case
                   of a non-Anoma event.
    - `:body` - A body of the event.
    """

    field(:node_id, any())
    field(:body, term())
  end

  defmacro new_with_body(node_id, body) do
    quote do
      %EventBroker.Event{
        source_module: __MODULE__,
        body: %Anoma.Controller.Event{
          node_id: unquote(node_id),
          body: unquote(body)
        }
      }
    end
  end

  deffilter ControllerFilter, node_id: String.t() do
    %EventBroker.Event{body: %Anoma.Controller.Event{node_id: ^node_id}} ->
      true

    _ ->
      false
  end

  @spec node_filter(String.t()) :: ControllerFilter.t()
  def node_filter(node_id) do
    %__MODULE__.ControllerFilter{node_id: node_id}
  end
end
