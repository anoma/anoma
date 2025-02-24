defmodule Examples.EEventBroker.Subscribe do
  @moduledoc """
  I define examples on how to susbcribe to topics in the event broker.
  """

  alias Examples.EEVentBroker.EFilter
  alias EventBroker.Event

  use ExUnit.Case

  # A list of default filters to use in the examples below.
  @filters [%EFilter.AcceptAll{}, %EFilter.Error{}]

  @doc """
  I subscribe using the `Trivial` filter and assert that I receive any events
  sent on the message broker.
  """
  @spec subscribe_to_filter(struct()) :: {:received, any()}
  def subscribe_to_filter(filter \\ %EFilter.AcceptAll{}) do
    # subscribe to the trivial filter (i.e., all messages)
    EventBroker.subscribe_me([filter])

    # create an event that matches the filter
    event = %Event{source_module: nil, body: %{message: "everything matches"}}

    # send the event
    EventBroker.event(event)

    # assert that this process receives the event
    assert_receive ^event

    EventBroker.unsubscribe_me([filter])

    {:received, event}
  end

  @doc """
  I subscribe to multiple filters and assert that I receive the events that are
  allowed by those filters.
  """

  @spec subscribe_to_multiple_filters([struct()]) :: {:received, any()}
  def subscribe_to_multiple_filters(filters \\ @filters) do
    # subscribe to the trivial filter (i.e., all messages)
    EventBroker.subscribe_me(filters)

    # confirm this process is subscribed
    assert_subscription(filters)

    # create an event that matches the filter
    event = %Event{source_module: nil, body: %{level: :error}}

    # send the event
    :ok = EventBroker.event(event)

    # assert that this process receives the event
    assert_receive ^event

    EventBroker.unsubscribe_me(filters)

    # confirm this process is unsubscribed
    refute_subscription(filters)

    {:received, event}
  end

  @doc """
  I unsubscribe a process from a filter and verify that it is unsubscribed.
  """
  @spec unsubscribe_from_filter(struct()) :: {:received, any()}
  def unsubscribe_from_filter(filter \\ %EFilter.AcceptAll{}) do
    # subscribe to the trivial filter (i.e., all messages)
    EventBroker.subscribe_me([filter])

    # create an event that matches the filter
    event = %Event{source_module: nil, body: %{message: "everything matches"}}

    # send the event
    EventBroker.event(event)

    # assert that this process receives the event
    assert_receive ^event

    EventBroker.unsubscribe_me([filter])

    {:received, event}
  end

  @doc """
  I subscribe a process to events, and check whether the registry cleans up its
  subscriptions when it goes offline.
  """
  def unsubscribe_on_down() do
    # any filter will do
    filter = %EFilter.AcceptAll{}

    this = self()

    # create a process that will subscribe
    subscriber =
      spawn(fn ->
        EventBroker.subscribe_me([filter])

        # confirm this process is subscribed
        assert_subscription(filter)

        # message the broker we're done
        send(this, :done)

        # wait for a message to terminate
        receive do
          :terminate ->
            :ok
        end
      end)

    # monitor the subscriber
    Process.monitor(subscriber)

    # wait for the subscriber to subscribe
    assert_receive :done

    # check that the process has been subscribed
    assert [[filter]] == EventBroker.subscriptions(subscriber)

    # stop the subscriber
    send(subscriber, :terminate)

    # wait for its down message
    assert_receive {:DOWN, _, _, ^subscriber, _}

    # assert that its no longer subscribed
    assert [] == EventBroker.subscriptions(subscriber)
  end

  ############################################################
  #                       Helpers                            #
  ############################################################
  @doc """
  I assert that the current process is subscribed to the given filter.
  """
  @spec assert_subscription([struct()] | struct()) :: [struct()] | struct()
  def assert_subscription(filter) when is_struct(filter) do
    # fetch the current subscriptions
    current_subscriptions = EventBroker.my_subscriptions()

    # check that we're currently not subscribed to this filter
    assert [filter] in current_subscriptions

    filter
  end

  def assert_subscription(filters) when is_list(filters) do
    # fetch the current subscriptions
    current_subscriptions = EventBroker.my_subscriptions()

    # check that we're currently not subscribed to this filter
    assert filters in current_subscriptions

    filters
  end

  @doc """
  Given a list of filters, I make sure that the current process is not subscribed to them.
  """
  @spec refute_subscription([struct()] | struct()) :: [struct()] | struct()
  def refute_subscription(filters) when is_list(filters) do
    # fetch the current subscriptions
    current_subscriptions = EventBroker.my_subscriptions()

    # check that we're currently not subscribed to this filter
    assert filters not in current_subscriptions

    filters
  end

  @spec refute_subscription(struct()) :: struct()
  def refute_subscription(filter) when is_struct(filter) do
    # fetch the current subscriptions
    current_subscriptions = EventBroker.my_subscriptions()

    # check that we're currently not subscribed to this filter
    assert [filter] not in current_subscriptions

    filter
  end
end
