defmodule Examples.EventBroker do
  @moduledoc false

  def start_broker do
    with {:ok, broker_pid} <- EventBroker.start_link(),
         {:ok, registry_pid} <- EventBroker.Registry.start_link(broker_pid) do
      {:ok, %{broker: broker_pid, registry: registry_pid}}
    else
      _ -> :error
    end
  end

  def trivial_filter_spec do
    %EventBroker.FilterSpec{
      filter_module: EventBroker.Filters.Trivial,
      filter_params: %EventBroker.Filters.Trivial{}
    }
  end

  def this_module_filter_spec do
    %EventBroker.FilterSpec{
      filter_module: EventBroker.Filters.SourceModule,
      filter_params: %EventBroker.Filters.SourceModule{module: __MODULE__}
    }
  end

  def example_message_a do
    %EventBroker.Event{
      source_module: __MODULE__,
      body: "example body"
    }
  end

  def example_message_b do
    %EventBroker.Event{
      source_module: Bad.Module,
      body: "example body"
    }
  end

  def subscribe_and_check do
    {:ok, %{broker: broker_pid, registry: registry_pid}} = start_broker()

    GenServer.cast(
      registry_pid,
      {:subscribe, self(),
       [
         trivial_filter_spec(),
         this_module_filter_spec(),
         trivial_filter_spec()
       ]}
    )

    # for dirty synchronization
    GenServer.call(registry_pid, :dump)

    send(broker_pid, example_message_a())
    send(broker_pid, example_message_b())

    receive do
      event = %EventBroker.Event{} ->
        {:ok, event}

      _ ->
        :error
    end
  end

  def million_messages(num_filters) do
    {:ok, %{broker: broker_pid, registry: registry_pid}} = start_broker()

    GenServer.cast(
      registry_pid,
      {:subscribe, self(),
       for _ <- 1..num_filters do
         this_module_filter_spec()
       end}
    )

    GenServer.call(registry_pid, :dump)

    f = fn ->
      for _ <- 1..1_000_000 do
        send(broker_pid, example_message_a())

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

    :timer.tc(f)
  end
end
