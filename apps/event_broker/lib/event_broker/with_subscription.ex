defmodule EventBroker.WithSubscription do
  @moduledoc """
  I contain the with_subscription macro. Require me or use me to use with_subscription.
  """

  defmacro __using__(_opts) do
    quote do
      import EventBroker.WithSubscription
    end
  end

  defmacro with_subscription(filter_spec_lists \\ [], do: logic) do
    Task.async(fn ->
      quote do
        for filter <- unquote(filter_spec_lists) do
          EventBroker.subscribe_me(filter)
        end

        res = unquote(logic)

        for filter <- unquote(filter_spec_lists) do
          EventBroker.unsubscribe_me(filter)
        end

        res
      end
    end)
    |> Task.await()
  end
end
