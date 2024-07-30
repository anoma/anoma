defmodule Examples.EventBroker do
  @moduledoc false

  require ExUnit.Assertions
  import ExUnit.Assertions

  def start_broker do
    on_start =
      with {:ok, broker_pid} <- EventBroker.start_link(),
           {:ok, registry_pid} <- EventBroker.Registry.start_link(broker_pid) do
        {:ok, %{broker: broker_pid, registry: registry_pid}}
      else
        {:error, {:already_started, _pid}} -> :already_started
        _ -> :error
      end

    assert on_start != :error

    on_start
  end

  def trivial_filter_spec do
    %EventBroker.Filters.Trivial{}
  end

  def this_module_filter_spec do
    %EventBroker.Filters.SourceModule{module: __MODULE__}
  end

  def example_message_a(string \\ "example body") do
    %EventBroker.Event{
      source_module: __MODULE__,
      body: string
    }
  end

  def example_message_b(string \\ "example body") do
    %EventBroker.Event{
      source_module: Bad.Module,
      body: string
    }
  end

  def subscribe_and_check do
    EventBroker.Registry.subscribe_me([
      trivial_filter_spec(),
      this_module_filter_spec(),
      trivial_filter_spec()
    ])

    EventBroker.event(example_message_a())
    EventBroker.event(example_message_b())

    {:ok, event} =
      receive do
        event = %EventBroker.Event{} ->
          {:ok, event}

        _ ->
          :error
      end

    EventBroker.Registry.unsubscribe_me([
      trivial_filter_spec(),
      this_module_filter_spec(),
      trivial_filter_spec()
    ])

    {:received, event}
  end

  def million_messages(num_filters) do
    filter_spec_list =
      for _ <- 1..num_filters do
        this_module_filter_spec()
      end

    EventBroker.Registry.subscribe_me(filter_spec_list)

    f = fn ->
      for _ <- 1..1_000_000 do
        EventBroker.event(example_message_a())

        {:ok, _} =
          receive do
            event = %EventBroker.Event{} ->
              {:ok, event}

            _ ->
              :error
          end
      end

      :success
    end

    result = :timer.tc(f)

    EventBroker.Registry.unsubscribe_me(filter_spec_list)

    result
  end

  def unsub_all() do
    start_broker()

    filters =
      :sys.get_state(EventBroker.Registry).registered_filters |> Map.keys()

    for f <- filters do
      EventBroker.Registry.unsubscribe_me(f)
    end

    assert [[]] ==
             :sys.get_state(EventBroker.Registry).registered_filters
             |> Map.keys()
  end

  def check_self_sub(list \\ []) do
    unsub_all()
    assert :ok = EventBroker.Registry.subscribe_me(list)

    agent =
      :sys.get_state(EventBroker.Registry).registered_filters |> Map.get(list)

    assert Process.alive?(agent)

    assert MapSet.new([self()]) == :sys.get_state(agent).subscribers

    {:sys.get_state(EventBroker.Registry), agent}
  end

  def check_sub_no_unsub(list \\ []) do
    start_broker()
    EventBroker.Registry.subscribe_me(list)

    agent =
      :sys.get_state(EventBroker.Registry).registered_filters |> Map.get(list)

    assert Process.alive?(agent)

    assert MapSet.new([self()])
           |> MapSet.subset?(:sys.get_state(agent).subscribers)

    {:sys.get_state(EventBroker.Registry), agent}
  end

  def message_works_trivial(string \\ "subscribing works") do
    trivial = [trivial_filter_spec()]
    {_, trivial_agent} = check_self_sub(trivial)

    message = string |> example_message_a()

    EventBroker.event(message)

    assert_receive ^message

    {:sys.get_state(EventBroker.Registry), trivial, trivial_agent}
  end

  def un_subscribing_works_atomic(list \\ [trivial_filter_spec()]) do
    {state, pid} = check_self_sub(list)

    EventBroker.Registry.unsubscribe_me(list)

    refute Process.alive?(pid)
    refute Map.get(state, list)

    :sys.get_state(EventBroker.Registry)
  end

  def message_gets_blocked(string \\ "blocked message") do
    module_spec = [this_module_filter_spec()]

    {_, pid} = check_self_sub(module_spec)

    message = string |> example_message_b()

    EventBroker.event(message)

    refute_receive ^message

    {:sys.get_state(EventBroker.Registry), module_spec, pid}
  end

  def add_filter_on_top(
        filter1 \\ [trivial_filter_spec()],
        filter2 \\ [trivial_filter_spec()]
      ) do
    filter = filter1 ++ filter2

    {_, pid1} = check_self_sub(filter1)
    {state, pid2} = check_sub_no_unsub(filter)

    registered = state.registered_filters

    pid_top_sub = Map.get(registered, filter1 ++ [hd(filter2)])

    get_agent = Map.get(registered, filter1)

    assert get_agent == pid1

    assert MapSet.new([self(), pid_top_sub]) ==
             :sys.get_state(get_agent).subscribers

    {:sys.get_state(EventBroker.Registry), filter, pid2}
  end

  def complex_filter_message(string \\ "complex filter message") do
    add_filter_on_top([trivial_filter_spec()], [
      this_module_filter_spec(),
      trivial_filter_spec()
    ])

    good_msg = string <> "good"
    bad_msg = string <> "bad"

    good_msg |> example_message_a() |> EventBroker.event()
    bad_msg |> example_message_b() |> EventBroker.event()

    assert_receive ^good_msg
    refute_receive ^bad_msg

    :sys.get_state(EventBroker.Registry)
  end

  def non_filters_fail() do
    unsub_all()

    assert "[Nock] do not export filtering functions" ==
             EventBroker.Registry.subscribe_me([%Nock{}])
  end
end
