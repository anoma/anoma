defmodule Anoma.Node.Events.Filters do
  use EventBroker.DefFilter
  alias Anoma.Node.Events.IntentAddSuccess
  alias Anoma.Node.Events.IntentAddError

  # @doc """
  # This filter will only subscribe to events regarding the Intent pool.
  # """
  deffilter Intentpool do
    %{body: %{body: %IntentAddSuccess{}}} ->
      true

    %{body: %{body: %IntentAddError{}}} ->
      true

    _ ->
      false
  end
end
