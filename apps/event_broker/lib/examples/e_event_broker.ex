defmodule Examples.EEventBroker do
  @moduledoc false

  alias EventBroker.Event
  alias EventBroker.Filters
  alias EventBroker.Registry
  alias EventBroker.Supervisor

  use ExUnit.Case

  @doc """
  I start the broker-registry duo.

  If no Broker or Registry were started beforehand, I return the message
  with their PIDs. If some was already started, return `:already_started`.
  Otherwise I error.

  I assert that error was not encountered and return the value.
  """

  @spec start_broker() ::
          :already_started | :error | {:ok, %{broker: pid(), registry: pid()}}
  def start_broker do
    on_start =
      with {:ok, sup_pid} <- Supervisor.start_link() do
        {:ok, sup_pid}
      else
        {:error, {:already_started, _pid}} -> :already_started
        _ -> :error
      end

    assert on_start != :error

    on_start
  end

  @doc """
  I am the trivial filter structure. See `EventBroker.Filters.Trivial`
  """

  @spec trivial_filter_spec() :: EventBroker.Filters.Trivial.t()
  def trivial_filter_spec do
    %EventBroker.Filters.Trivial{}
  end

  @doc """
  I am the filter structure corresponding to a filter which sorts messages
  depending on whether they come from the module which calls the function.

  See `EventBroker.Filters.SourceModule`
  """

  @spec this_module_filter_spec() :: EventBroker.Filters.SourceModule.t()
  def this_module_filter_spec do
    %EventBroker.Filters.SourceModule{module: __MODULE__}
  end

  @doc """
  I am an example message. Given a string, I return the event message for
  the Broker-specified format, where the source module is given by the
  module which sent the message and with body the specified string.
  """

  @spec example_message_a(String.t()) :: Event.t()
  @spec example_message_a() :: Event.t()
  def example_message_a(string \\ "example body") do
    %Event{
      source_module: __MODULE__,
      body: string
    }
  end

  @doc """
  I am an example message whose source module is `Bad.Module`.
  """

  @spec example_message_b(String.t()) :: Event.t()
  @spec example_message_b() :: Event.t()
  def example_message_b(string \\ "example body") do
    %Event{
      source_module: Bad.Module,
      body: string
    }
  end

  @doc """
  I am a function which test complex subscription.

  I subscribe to a chain:
  trivial_filter -> module_check_filer -> trivial_filter

  I match the event received, unsubscribe from the filter, then return the
  received event.
  """

  @spec subscribe_and_check() :: {:received, Event.t()}
  def subscribe_and_check do
    EventBroker.subscribe_me([
      trivial_filter_spec(),
      this_module_filter_spec(),
      trivial_filter_spec()
    ])

    EventBroker.event(example_message_a())
    EventBroker.event(example_message_b())

    {:ok, event} =
      receive do
        event = %Event{} ->
          {:ok, event}

        _ ->
          :error
      end

    EventBroker.unsubscribe_me([
      trivial_filter_spec(),
      this_module_filter_spec(),
      trivial_filter_spec()
    ])

    {:received, event}
  end

  @doc """
  I am a function which sends a million messages through a specified number
  of filters.

  Given a number `n`, I spawn an n-long chain of module-checking filters
  subscribed to each other, send a million messages to the Broker, then
  wait to see how long it takes to finish receiving them all.

  I then unsubscribe and return the time.
  """

  @spec million_messages(integer()) :: {integer(), any()}
  def million_messages(num_filters) do
    filter_spec_list =
      for _ <- 1..num_filters do
        this_module_filter_spec()
      end

    EventBroker.subscribe_me(filter_spec_list)

    f = fn ->
      for _ <- 1..1_000_000 do
        EventBroker.event(example_message_a())

        {:ok, _} =
          receive do
            event = %Event{} ->
              {:ok, event}

            _ ->
              :error
          end
      end

      :success
    end

    result = :timer.tc(f)

    EventBroker.unsubscribe_me(filter_spec_list)

    result
  end

  @doc """
  I am a function unsubbing from all filters.

  Assuming that only one non-filter actor is subscribed to the list of the
  filter tree, I start the broker, then see all the filters. If they have
  self as a subscriber, I ask the Registry to unsubscribe.

  Per the hypothesis, only the Broker should remain alive after that. I
  check that this is indeed true and return the Registry state.
  """

  @spec unsub_all() :: Registry.t()
  def unsub_all() do
    start_broker()

    # unsubscribe from all my current subscriptions
    EventBroker.my_subscriptions()
    |> Enum.each(&EventBroker.unsubscribe_me(&1))

    assert EventBroker.my_subscriptions() == []

    :sys.get_state(Registry)
  end

  @doc """
  I check that basic subscribing works.

  Given a filter spec, I first unsub from everything using `unsub_all/1`
  then ask the Registry to subscribe me to the said filter as specified.

  I then check through Registry that the filter has indeed been added, that
  it is alive, and that its only subscriber is self.

  I return a tuple with the Registry state and the agent.
  """

  @spec check_self_sub(list()) :: {Registry.t(), pid()}
  @spec check_self_sub() :: {Registry.t(), pid()}
  def check_self_sub(list \\ []) do
    unsub_all()
    assert :ok = EventBroker.subscribe_me(list)

    agent =
      :sys.get_state(Registry).registered_filters |> Map.get(list)

    assert Process.alive?(agent)

    assert self() in :sys.get_state(agent).subscribers

    {:sys.get_state(Registry), agent}
  end

  @doc """
  I am a function checking subscription mechanism without unsubbing.

  I act similar to `check_self_sub/1` only I do not unsub from all the
  filters upon start. I just start the broker and sub to the filter.

  I check that it has been registered, that it is alive, and that self is
  one of the subscribers.
  """

  @spec check_sub_no_unsub(list()) :: {Registry.t(), pid()}
  @spec check_sub_no_unsub() :: {Registry.t(), pid()}
  def check_sub_no_unsub(list \\ []) do
    start_broker()
    EventBroker.subscribe_me(list)

    agent =
      :sys.get_state(Registry).registered_filters |> Map.get(list)

    assert Process.alive?(agent)

    assert MapSet.new([self()])
           |> MapSet.subset?(:sys.get_state(agent).subscribers)

    {:sys.get_state(Registry), agent}
  end

  @doc """
  I check that the trivial message sort works.

  I start the trivial filter using `check_self_sub/1` and then send an
  event message using the specified string. I then assert that the message
  was indeed received and return the Registry state, the specified filter,
  as well as the PID of said filter.
  """

  @spec message_works_trivial(String.t()) ::
          {Registry.t(), list(), pid()}
  @spec message_works_trivial() :: {Registry.t(), list(), pid()}
  def message_works_trivial(string \\ "subscribing works") do
    trivial = [trivial_filter_spec()]
    {_, trivial_agent} = check_self_sub(trivial)

    message = string |> example_message_a()

    EventBroker.event(message)

    assert_receive ^message

    {:sys.get_state(Registry), trivial, trivial_agent}
  end

  @doc """
  I atomically check that unsubscription works.

  Given a filter spec, I ask to subscribe to it using `check_self_sub/1` to
  then ask the Registry to unsubscribe self from it. Afterwards, I check
  that the originally registered PID is dead and the process is
  deregistered.

  I then return the Registry state.
  """

  @spec un_subscribing_works_atomic(list()) :: Registry.t()
  @spec un_subscribing_works_atomic() :: Registry.t()
  def un_subscribing_works_atomic(list \\ [trivial_filter_spec()]) do
    {state, pid} = check_self_sub(list)

    EventBroker.unsubscribe_me(list)

    refute Process.alive?(pid)
    refute Map.get(state, list)

    :sys.get_state(Registry)
  end

  @doc """
  I check that the filter blocking messages based on source works.

  I use `check_self_sub/1` to subscribe to the filter which filters based
  on whether the event source was the module I am called from. Then I send
  the message with source from a different module and refute that it was
  ever received.

  I return a tuple with the Registry state, the filter spec, and its PID.
  """

  @spec message_gets_blocked(String.t()) ::
          {Registry.t(), [Filters.SourceModule.t()], pid()}
  @spec message_gets_blocked() ::
          {Registry.t(), [Filters.SourceModule.t()], pid()}
  def message_gets_blocked(string \\ "blocked message") do
    module_spec = [this_module_filter_spec()]

    {_, pid} = check_self_sub(module_spec)

    message = string |> example_message_b()

    EventBroker.event(message)

    refute_receive ^message

    {:sys.get_state(Registry), module_spec, pid}
  end

  @doc """
  I check that the filters get added on top of one another if some part of
  its dependency chain has already been registered.

  Given two lists of filter specs, I start the one gitten by appending both
  specs by first using `check_self_sub/1` on the first input and then the
  `check_sub_no_usub/1` on the second. I then check that the the head of
  the second input spec was indeed subscribed to the last element of the
  first input along with ourselves and that the PIDs match on initial
  filter startup and the final registry for the first input.
  """

  @spec add_filter_on_top(list(), list()) ::
          {Registry.t(), list(), pid()}
  @spec add_filter_on_top(list()) :: {Registry.t(), list(), pid()}
  @spec add_filter_on_top() :: {Registry.t(), list(), pid()}
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

    {:sys.get_state(Registry), filter, pid2}
  end

  @doc """
  I check that a complex filter chain works for filtering messages.

  I fist use `add_filter_on_top/2` to start a dependency chain:
  trivial_filter -> module_filter -> trivial_filter

  Afterwards, I unsubscribe from the top trivial filter, and send two
  messages. One is sent from self, so it should pass all filters.

  The other is sent from another module, so it should not get received in
  the end. I assert that I receive the first and refute that I receive the
  second one and return the Registry state.
  """

  @spec complex_filter_message(String.t()) :: Registry.t()
  @spec complex_filter_message() :: Registry.t()
  def complex_filter_message(string \\ "complex filter message") do
    trivial = [trivial_filter_spec()]

    add_filter_on_top(trivial, [
      this_module_filter_spec(),
      trivial_filter_spec()
    ])

    good_msg = (string <> " good") |> example_message_a()
    bad_msg = (string <> " bad") |> example_message_b()

    EventBroker.unsubscribe_me(trivial)

    EventBroker.event(good_msg)
    EventBroker.event(bad_msg)

    assert_receive ^good_msg
    refute_receive ^bad_msg

    :sys.get_state(Registry)
  end

  @doc """
  I check that modules with no filters cannot be registered.

  I unsub from all filters using `unsub_all/0` and then try to subscribe
  self to the filter specified by the Nock module. As the Nock module has
  no filters, it should return an error message. I assert that the correct
  error message got returned.
  """

  @spec non_filters_fail() :: true
  def non_filters_fail() do
    unsub_all()

    assert "[Nock] do not export filtering functions" ==
             EventBroker.subscribe_me([%{__struct__: Nock}])
  end

  @doc """
  I am a function that tests the unsubscription mechanism when a subscriber goes offline.

  I create two processes to subscribe to a chain, and then kill them one by one.

  I check that the filters remain in the registry after the first subscriber is killed,
  and that they are removed after the second subscriber is killed.
  """

  @spec kill_subscriber() :: boolean()
  def kill_subscriber do
    # assert that the registry is now empty
    assert [[]] ==
             :sys.get_state(EventBroker.Registry).registered_filters
             |> Map.keys()

    # get the pid of the shell to wait for processes to be ready
    this = self()
    # spawn a first subscriber
    first =
      spawn(fn ->
        Process.register(self(), :first)

        :ok =
          EventBroker.subscribe_me([
            trivial_filter_spec()
          ])

        send(this, :first)

        rcv = fn rcv ->
          receive do
            %Event{} -> :ok
          end

          rcv.(rcv)
        end

        rcv.(rcv)
      end)

    second =
      spawn(fn ->
        Process.register(self(), :second)

        :ok =
          EventBroker.subscribe_me([
            trivial_filter_spec()
          ])

        send(this, :second)

        rcv = fn rcv ->
          receive do
            %Event{} -> :ok
          end

          rcv.(rcv)
        end

        rcv.(rcv)
      end)

    receive do
      :first -> :ok
    end

    receive do
      :second -> :ok
    end

    assert [first, second] ==
             Map.keys(
               :sys.get_state(EventBroker.Registry).registered_subscribers
             )

    # kill the first subscriber
    Process.exit(first, :kill)
    Process.sleep(100)

    assert [[], [%EventBroker.Filters.Trivial{}]] ==
             Map.keys(:sys.get_state(EventBroker.Registry).registered_filters)

    # kill the first subscriber
    Process.exit(second, :kill)
    Process.sleep(100)

    assert [[]] ==
             Map.keys(:sys.get_state(EventBroker.Registry).registered_filters)
  end
end
