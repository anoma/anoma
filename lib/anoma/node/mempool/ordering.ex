defmodule Anoma.Node.Mempool.Ordering do
  @moduledoc """
  abordering genserver
  """

  use GenServer
  use TypedStruct

  alias __MODULE__
  alias Anoma.Node.Mempool
  alias Mempool.Storage

  require EventBroker.Event

  typedstruct enforce: true do
    field(:next_height, integer(), default: 1)
    field(:hash_to_order, %{binary() => integer()}, default: %{})
  end

  typedstruct enforce: true, module: OrderEvent do
    field(:id, integer())
  end

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: __MODULE__)
  end

  def init(_opts) do
    {:ok, %Ordering{}}
  end

  def handle_call({:read, {id, key}}, from, state) do
    IO.puts("==============Read Reached=============")

    with {:ok, height} <- Map.fetch(state.hash_to_order, id) do
      {:ok, value} = Storage.read({height, key})
      {:reply, {:ok, value}, state}
    else
      _ ->
        {:ok, pid} = Task.start(fn -> blocking_read(id, key, from) end)

        EventBroker.subscribe(pid, [
          this_module_filter(),
          id_filter(id)
        ])

        {:noreply, state}
    end
  end

  def handle_call({:write, {id, key}, value}, from, state) do
    IO.puts("==============Write Reached Ordering=============")

    with {:ok, height} <- Map.fetch(state.hash_to_order, id) do
      :ok = Storage.write({height, key}, value)
      IO.puts("=============STORAGE RETURN WRITE VALUE TO ORDER==========")
      {:reply, :ok, state}
    else
      _ ->
        {:ok, pid} =
          Task.start(fn -> blocking_write(id, key, value, from) end)

        EventBroker.subscribe(pid, [
          this_module_filter(),
          id_filter(id)
        ])

        {:noreply, state}
    end
  end

  def handle_call({:order, txs}, from, state) do
    {to_order, _left} = txs |> Enum.shuffle() |> Enum.split(10)

    {map, next_order} =
      for id <- to_order,
          reduce: {state.hash_to_order, state.next_height} do
        {map, order} ->
          {Map.put(map, id, order), order + 1}
      end

    {:ok, pid} = Task.start(fn -> blocking_complete(to_order, map, from) end)

    for id <- to_order do
      EventBroker.subscribe(pid, [
        worker_module_filter(),
        id_filter(id)
      ])

      order_event =
        EventBroker.Event.new_with_body(%__MODULE__.OrderEvent{
          id: id
        })

      EventBroker.event(order_event)
    end

    {:noreply,
     %__MODULE__{state | hash_to_order: map, next_height: next_order}}
  end

  def handle_call(_msg, _from, state) do
    {:reply, :ok, state}
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end

  def handle_info(_info, state) do
    {:noreply, state}
  end

  def read({id, key}) do
    IO.puts("==============READ CALL Reached=============")
    GenServer.call(__MODULE__, {:read, {id, key}}, :infinity)
  end

  def write({id, key}, value) do
    IO.puts("==============Write CALL Reached=============")
    GenServer.call(__MODULE__, {:write, {id, key}, value}, :infinity)
  end

  def order(txs) do
    IO.puts("==============Order CALL Reached=============")
    GenServer.call(__MODULE__, {:order, txs})
  end

  def blocking_read(id, key, from) do
    receive do
      %EventBroker.Event{body: %__MODULE__.OrderEvent{id: ^id}} ->
        GenServer.reply(from, read({id, key}))
    end

    EventBroker.unsubscribe_me([
      this_module_filter(),
      id_filter(id)
    ])
  end

  def blocking_write(id, key, value, from) do
    receive do
      %EventBroker.Event{body: %__MODULE__.OrderEvent{id: ^id}} ->
        GenServer.reply(from, write({id, key}, value))
    end

    EventBroker.unsubscribe_me([
      this_module_filter(),
      id_filter(id)
    ])
  end

  def blocking_complete(ids, map, from) do
    res_list =
      for _id <- ids,
          reduce: [] do
        list ->
          receive do
            %EventBroker.Event{
              body: %Anoma.Node.Mempool.Backends.CompleteEvent{
                id: id,
                result: :ok
              }
            } ->
              EventBroker.unsubscribe_me([
                worker_module_filter(),
                id_filter(id)
              ])

              [{:ok, id, Map.get(map, id)} | list]

            %EventBroker.Event{
              body: %Anoma.Node.Mempool.Backends.CompleteEvent{
                id: id,
                result: :error
              }
            } ->
              EventBroker.unsubscribe_me([
                worker_module_filter(),
                id_filter(id)
              ])

              [{:error, id, Map.get(map, id)} | list]
          end
      end

    GenServer.reply(from, res_list)
  end

  def this_module_filter() do
    %EventBroker.Filters.SourceModule{module: __MODULE__}
  end

  def worker_module_filter() do
    %EventBroker.Filters.SourceModule{module: Anoma.Node.Mempool.Backends}
  end

  def id_filter(id) do
    %__MODULE__.IdFilter{id: id}
  end
end
