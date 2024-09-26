defmodule Anoma.Node.Transaction.Ordering do
  @moduledoc """
  abordering genserver
  """

  use GenServer
  use TypedStruct

  alias __MODULE__
  alias Anoma.Node.Transaction.Storage

  require EventBroker.Event

  typedstruct enforce: true do
    field(:next_height, integer(), default: 1)
    # maps tx ids to their height for writing.
    # the previous height is used for reading.
    field(:tx_id_to_height, %{binary() => integer()}, default: %{})
  end

  typedstruct enforce: true, module: OrderEvent do
    field(:tx_id, binary())
  end

  def start_link(args \\ []) do
    GenServer.start_link(__MODULE__, args, name: __MODULE__)
  end

  def init(args) do
    {:ok, keylist} = args |> Keyword.validate(next_height: 1)
    {:ok, %Ordering{next_height: keylist[:next_height]}}
  end

  def handle_call({:read, {tx_id, key}}, from, state) do
    with {:ok, height} <- Map.fetch(state.tx_id_to_height, tx_id) do
      Task.start(fn ->
        GenServer.reply(from, Storage.read({height - 1, key}))
      end)

      {:noreply, state}
    else
      _ ->
        block_spawn(tx_id, fn -> blocking_read({tx_id, key}, from) end)
        {:noreply, state}
    end
  end

  def handle_call({:write, {tx_id, kvlist}}, from, state) do
    with {:ok, height} <- Map.fetch(state.tx_id_to_height, tx_id) do
      Task.start(fn ->
        GenServer.reply(from, Storage.write({height, kvlist}))
      end)

      {:noreply, state}
    else
      _ ->
        block_spawn(tx_id, fn -> blocking_write({tx_id, kvlist}, from) end)

        {:noreply, state}
    end
  end

  def handle_call(_msg, _from, state) do
    {:reply, :ok, state}
  end

  def handle_cast({:order, tx_id_list}, state) do
    {map, next_order} =
      for tx_id <- tx_id_list,
          reduce: {state.tx_id_to_height, state.next_height} do
        {map, order} ->
          order_event =
            EventBroker.Event.new_with_body(%__MODULE__.OrderEvent{
              tx_id: tx_id
            })

          EventBroker.event(order_event)
          {Map.put(map, tx_id, order), order + 1}
      end

    {:noreply,
     %__MODULE__{state | tx_id_to_height: map, next_height: next_order}}
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end

  def handle_info(_info, state) do
    {:noreply, state}
  end

  def block_spawn(id, call) do
    {:ok, pid} =
      Task.start(call)

    EventBroker.subscribe(pid, [
      this_module_filter(),
      tx_id_filter(id)
    ])
  end

  def read({id, key}) do
    GenServer.call(__MODULE__, {:read, {id, key}}, :infinity)
  end

  @spec write({binary(), list({any(), any()})}) :: :ok
  def write({id, kvlist}) do
    GenServer.call(__MODULE__, {:write, {id, kvlist}}, :infinity)
  end

  def order(txs) do
    GenServer.cast(__MODULE__, {:order, txs})
  end

  def blocking_read({id, key}, from) do
    block(from, id, fn -> read({id, key}) end)
  end

  def blocking_write({id, kvlist}, from) do
    block(from, id, fn ->
      write({id, kvlist})
    end)
  end

  def block(from, tx_id, call) do
    receive do
      %EventBroker.Event{body: %__MODULE__.OrderEvent{tx_id: ^tx_id}} ->
        result = call.()
        GenServer.reply(from, result)

      _ ->
        IO.puts("this should be unreachable")
    end

    EventBroker.unsubscribe_me([
      this_module_filter(),
      tx_id_filter(tx_id)
    ])
  end

  def this_module_filter() do
    %EventBroker.Filters.SourceModule{module: __MODULE__}
  end

  def tx_id_filter(tx_id) do
    %__MODULE__.TxIdFilter{tx_id: tx_id}
  end
end
