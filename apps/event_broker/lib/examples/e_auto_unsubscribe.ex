defmodule Examples.EEventBroker.AutoUnsubscribe do
  @moduledoc """
  I contain some examples that show that processes that terminate are automatically unsubscribed.
  """

  alias EventBroker.Event
  alias EventBroker.Filters
  alias EventBroker.Registry
  alias EventBroker.Supervisor
  alias Examples.EEventBroker

  use ExUnit.Case

  import Anoma.Examples.Helpers

  def subscriber_count() do
    Enum.count(:sys.get_state(EventBroker.Registry).registered_subscribers)
  end

  def create_subscriber() do
    this = self()

    # spawn a new process to subscribe
    subscriber =
      spawn_link(fn ->
        EventBroker.subscribe_me([])
        send(this, :subscribed)

        receive do
          :terminate ->
            :ok
        end
      end)

    # wait for the subscriber to be subscribed
    receive do
      :subscribed ->
        :ok
    end

    subscriber
  end

  def terminate_subscriber(subscriber_pid) do
    send(subscriber_pid, :terminate)

    assert_eventually(fn ->
      assert not Process.alive?(subscriber_pid)
    end)
  end

  @spec count_filters :: [pid()]
  def count_filters(opts \\ []) do
    new_subscribers = Keyword.get(opts, :subscriber_count, 100)

    # get the current subscriber count
    before = subscriber_count()

    # start subscribers
    subs = Enum.map(1..new_subscribers, fn _ -> create_subscriber() end)

    # check that the subscriber count is correct
    assert subscriber_count() == new_subscribers + before

    subs
  end

  def kill_subscribers(opts \\ []) do
    subscribers = Keyword.get_lazy(opts, :subscribers, fn -> count_filters(opts) end)

    # get the current subscriber count
    before = subscriber_count()

    # kill all the subscribers
    Enum.each(subscribers, &terminate_subscriber/1)

    # verify count again
    assert subscriber_count() == before - Enum.count(subscribers)
  end

  def measure_subscription_time(opts \\ []) do
    # add the incremental number of subscribers
    subscribers = count_filters(opts)

    # do 10 subscribtions and take the average
    average_time = for i <- 1..10 do
      {time, pid} = :timer.tc(fn -> pid = create_subscriber() end)
      terminate_subscriber(pid)
      time
    end
    |> Enum.sum()
    |> Kernel./(10)

    # kill the previous subscribers
    kill_subscribers(subscribers: subscribers)

    average_time
  end
end
