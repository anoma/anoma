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
    quote do
      # only subscribe to non-existing subscriptions
      current_subscriptions = EventBroker.my_subscriptions()
      new_subscriptions = unquote(filter_spec_lists) -- current_subscriptions

      for filter <- new_subscriptions do
        EventBroker.subscribe_me(filter)
      end

      res = unquote(logic)

      for filter <- new_subscriptions do
        EventBroker.unsubscribe_me(filter)
      end

      res
    end
  end
end
