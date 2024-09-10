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
    field(:storage, pid())
    field(:mempool, Anoma.Node.Router.addr())
    field(:next_height, integer(), default: 1)
    field(:hash_to_order, %{binary() => integer()}, default: %{})
  end

  typedstruct enforce: true, module: OrderEvent do
    field(:id, integer())
  end

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: __MODULE__)
  end

  def init(opts) do
    {:ok,
     %Ordering{storage: Process.whereis(Storage), mempool: opts[:mempool]}}
  end

  def handle_call({:read, {id, key}}, from, state) do
    # do a with fetch
    if Map.get(state.order, id) do
      height = Map.get(state.order, id)
      {:ok, value} = Storage.read({height, key})
      {:reply, {:ok, value}, state}
    else
      {:ok, pid} = Task.start(fn -> blocking_read(id, key, from) end)

      EventBroker.subscribe(pid, [
        this_module_filter(),
        id_filter(id)
      ])

      {:noreply, state}
    end
  end

  def handle_call({:write, {id, key}, value}, from, state) do
    if Map.get(state.order, id) do
      height = Map.get(state.order, id)
      :ok = Storage.write({height, key}, value)
      {:reply, :ok, state}
    else
      {:ok, pid} = Task.start(fn -> blocking_read(id, key, from) end)

      EventBroker.subscribe(pid, [
        this_module_filter(),
        id_filter(id)
      ])

      {:noreply, state}
    end
  end

  def handle_call({:order, txs}, from, state) do
    {to_order, _left} = txs |> Enum.shuffle() |> Enum.split(10)

    {map, next_order, ids} =
      for {id, _val} <- to_order,
          reduce: {state.hash_to_order, state.next_height, []} do
        {map, order, ids} ->
          order_event =
            EventBroker.Event.new_with_body(%__MODULE__.OrderEvent{
              id: id
            })

          EventBroker.event(order_event)
          {Map.put(map, id, order), order + 1, [id | ids]}
      end

    {:ok, pid} = Task.start(fn -> blocking_complete(ids, map, from) end)

    for id <- ids do
      EventBroker.subscribe(pid, [
        worker_module_filter(),
        id_filter(id)
      ])
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
    GenServer.call(__MODULE__, {:read, {id, key}}, :infinity)
  end

  def write({id, key}, value) do
    GenServer.cast(__MODULE__, {:write, {id, key}, value})
  end

  def order(txs) do
    GenServer.call(__MODULE__, {:order, txs})
  end

  def blocking_read(id, key, _from) do
    receive do
      %EventBroker.Event{body: %__MODULE__.OrderEvent{id: ^id}} ->
        read({id, key})
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

    GenServer.reply(res_list, from)
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
