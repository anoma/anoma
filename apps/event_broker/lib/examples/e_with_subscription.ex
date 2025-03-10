defmodule Examples.EEVentBroker.WithSub do
  @moduledoc """
  I contain examples of how the `with_subscription` block works.
  """

  alias Examples.EEVentBroker.EFilter
  alias EventBroker.Event

  import Examples.EEventBroker.Subscribe

  use ExUnit.Case
  use EventBroker.WithSubscription

  # A list of default filters to use in the examples below.
  @filters [%EFilter.AcceptAll{}, %EFilter.Error{}]

  @doc """
  I run a piece of code with a subscription. I assert that the messages sent
  within this block are sent to me.
  """
  @spec with_subscription(struct()) :: struct()
  def with_subscription(filter \\ %EFilter.AcceptAll{}) do
    # make sure this process is not already subscribed
    refute_subscription(filter)

    with_subscription([[filter]]) do
      # create an event that matches the filter
      event = %Event{
        source_module: nil,
        body: %{message: "everything matches"}
      }

      # send the event
      EventBroker.event(event)

      # assert that this process receives the event
      assert_receive ^event
    end

    # make sure this process is not still subscribed
    refute_subscription(filter)

    filter
  end

  @doc """
  I run a piece of code with a subscription to multiple filters. I assert that
  the messages sent within this block are sent to me.
  """
  @spec with_subscriptions([struct()]) :: [struct()]
  def with_subscriptions(filters \\ @filters) do
    # make sure this process is not already subscribed
    refute_subscription(filters)

    with_subscription([filters]) do
      # create an event that matches the filter
      event = %Event{
        source_module: nil,
        body: %{level: :error}
      }

      # send the event
      EventBroker.event(event)

      # assert that this process receives the event
      assert_receive ^event
    end

    # make sure this process is not still subscribed
    refute_subscription(filters)

    filters
  end

  @doc """
  I check that, when a process is subscribed to a given filter, that filter
  still exists after a `with_subscription` block that uses the same filter.
  """
  @spec with_existing_subscription(struct()) :: struct()
  def with_existing_subscription(filter \\ %EFilter.AcceptAll{}) do
    # subscribe to the given filter
    EventBroker.subscribe_me([filter])

    # make sure this process is not already subscribed
    assert_subscription(filter)

    with_subscription([[filter]]) do
      assert_subscription(filter)
    end

    # make sure this process is still subscribed
    assert_subscription(filter)

    filter
  end

  @doc """
  I nest two with_subscription blocks and assert that the middle one is subscribed to the outer topics as well.
  """
  @spec with_nested_subscription([struct()]) :: [struct()]
  def with_nested_subscription([filter1, filter2] \\ @filters) do
    # no filter1
    refute_subscription(filter1)

    # no filter2
    refute_subscription(filter2)

    with_subscription([[filter1]]) do
      # confirm filter1
      assert_subscription(filter1)

      # no filter2
      refute_subscription(filter2)

      with_subscription([[filter2]]) do
        # confirm filter1
        assert_subscription(filter1)
        # confirm filter2
        assert_subscription(filter2)
      end

      # confirm filter1
      assert_subscription(filter1)

      # no filter2
      refute_subscription(filter2)
    end

    # no filter1
    refute_subscription(filter1)

    # no filter2
    refute_subscription(filter2)

    [filter1, filter2]
  end
end
