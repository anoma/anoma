defmodule Anoma.Node.Transaction.Storage do
  @moduledoc """
  abstorage genserver
  """

  use GenServer
  use TypedStruct
  require EventBroker.Event

  alias __MODULE__

  @type bare_key() :: list(String.t())
  @type qualified_key() :: {integer(), bare_key()}

  typedstruct enforce: true do
    field(:uncommitted, %{qualified_key() => term()}, default: %{})
    # the most recent height written.
    # starts at 0 because nothing has been written.
    field(:uncommitted_height, integer(), default: 0)
    # reverse-ordered list of heights at which a key was updated.
    field(:uncommitted_updates, %{bare_key() => list(integer())},
      default: %{}
    )
  end

  typedstruct enforce: true, module: WriteEvent do
    field(:height, integer())
    field(:writes, list({Anoma.Node.Transaction.Storage.bare_key(), term()}))
  end

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: __MODULE__)
  end

  def init(_) do
    :mnesia.create_table(__MODULE__.Values, attributes: [:key, :value])
    :mnesia.create_table(__MODULE__.Updates, attributes: [:key, :value])
    :mnesia.create_table(__MODULE__.Blocks, attributes: [:round, :block])
    {:ok, %Storage{}}
  end

  def handle_call({:commit, round, writes}, _from, state) do
    mnesia_tx = fn ->
      for {key, value} <- state.uncommitted do
        :mnesia.write({__MODULE__.Values, key, value})
      end

      for {key, value} <- state.uncommitted_updates do
        {:atomic, res} =
          fn -> :mnesia.read(__MODULE__.Updates, key) end
          |> :mnesia.transaction()

        new_updates =
          case res do
            [] -> value
            [{__MODULE__.Updates, _key, list}] -> value ++ list
          end

        :mnesia.write({__MODULE__.Updates, key, new_updates})
        :mnesia.write({__MODULE__.Blocks, round, writes})
      end
    end

    :mnesia.transaction(mnesia_tx)
    {:reply, :ok, %__MODULE__{uncommitted_height: state.uncommitted_height}}
  end

  def handle_call({:read, {0, _key}}, _from, state) do
    {:reply, :absent, state}
  end

  def handle_call({:read, {height, key}}, from, state) do
    if height <= state.uncommitted_height do
      # relies on this being a reverse-ordered list
      result =
        case Map.get(state.uncommitted_updates, key) do
          nil ->
            tx1 = fn -> :mnesia.read(__MODULE__.Updates, key) end

            {:atomic, tx1_result} =
              :mnesia.transaction(tx1)

            case tx1_result do
              [{__MODULE__.Updates, _key, height_upds}] ->
                height = height_upds |> Enum.find(fn a -> a <= height end)

                case height do
                  nil ->
                    :absent

                  _ ->
                    tx2 = fn ->
                      :mnesia.read(__MODULE__.Values, {height, key})
                    end

                    {:atomic,
                     [{__MODULE__.Values, {_height, _key}, tx2_result}]} =
                      :mnesia.transaction(tx2)

                    {:ok, tx2_result}
                end

              [] ->
                :absent

              _ ->
                :error
            end

          heights ->
            update_height = heights |> Enum.find(fn a -> a <= height end)

            case update_height do
              nil ->
                :absent

              _ ->
                {:ok,
                 Map.get(state.uncommitted, {update_height, key}, :error)}
            end
        end

      {:reply, result, state}
    else
      block_spawn(height, fn -> blocking_read(height, key, from) end)
      {:noreply, state}
    end
  end

  def handle_call({:write, {height, kvlist}}, from, state) do
    unless height == state.uncommitted_height + 1 do
      block_spawn(height - 1, fn ->
        blocking_write(height, kvlist, from)
      end)

      {:noreply, state}
    else
      new_state = abwrite(:write, {height, kvlist}, state)

      {:reply, :ok, new_state}
    end
  end

  def handle_call({:append, {height, kvlist}}, from, state) do
    unless height == state.uncommitted_height + 1 do
      block_spawn(height - 1, fn ->
        blocking_write(height, kvlist, from)
      end)

      {:noreply, state}
    else
      new_state = abwrite(:append, {height, kvlist}, state)

      {:reply, :ok, new_state}
    end
  end

  def handle_call(_msg, _from, state) do
    {:reply, :ok, state}
  end

  def block_spawn(height, call) do
    {:ok, pid} =
      Task.start(call)

    EventBroker.subscribe(pid, [
      this_module_filter(),
      height_filter(height)
    ])
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end

  def handle_info(_info, state) do
    {:noreply, state}
  end

  def read({height, key}) do
    GenServer.call(__MODULE__, {:read, {height, key}}, :infinity)
  end

  def write({height, kvlist}) do
    GenServer.call(__MODULE__, {:write, {height, kvlist}}, :infinity)
  end

  def commit(block_round, writes) do
    GenServer.call(__MODULE__, {:commit, block_round, writes})
  end

  defp blocking_read(height, key, from) do
    receive do
      # if the key we care about was written at exactly the height we
      # care about, then we already have the value for free
      %EventBroker.Event{
        body: %__MODULE__.WriteEvent{height: ^height, writes: writes}
      } ->
        case Enum.find(writes, fn {keywrite, _value} -> key == keywrite end) do
          # try reading in history instead
          nil ->
            GenServer.reply(from, read({height, key}))

          # return value
          {_key, value} ->
            GenServer.reply(from, {:ok, value})
        end

      _ ->
        IO.puts("this should be unreachable")
    end

    EventBroker.unsubscribe_me([
      this_module_filter(),
      height_filter(height)
    ])
  end

  # todo: should exclude same key being overwritten at same height
  def abwrite(flag, {height, kvlist}, state) do
    {new_state, event_writes} =
      for {key, value} <- kvlist,
          reduce: {state, kvlist} do
        {state_acc, list} ->
          key_old_updates = Map.get(state_acc.uncommitted_updates, key, [])
          key_new_updates = [height | key_old_updates]

          new_updates =
            Map.put(state_acc.uncommitted_updates, key, key_new_updates)

          {new_kv, event_writes_local} =
            case flag do
              :append ->
                old_set_value =
                  Map.get(state_acc.uncommitted_updates, key, MapSet.new())

                new_set_value = MapSet.put(old_set_value, value)

                new_kv =
                  Map.put_new(
                    state_acc.uncommitted,
                    {height, key},
                    new_set_value
                  )

                {new_kv, [{key, new_set_value} | list]}

              :write ->
                new_kv =
                  Map.put_new(state_acc.uncommitted, {height, key}, value)

                {new_kv, kvlist}
            end

          {%__MODULE__{
             state
             | uncommitted: new_kv,
               uncommitted_height: height,
               uncommitted_updates: new_updates
           }, event_writes_local}
      end

    write_event =
      EventBroker.Event.new_with_body(%__MODULE__.WriteEvent{
        height: height,
        writes: event_writes
      })

    EventBroker.event(write_event)

    new_state
  end

  def blocking_write(height, kvlist, from) do
    awaited_height = height - 1

    receive do
      %EventBroker.Event{
        body: %__MODULE__.WriteEvent{height: ^awaited_height}
      } ->
        GenServer.reply(from, write({height, kvlist}))
    end

    EventBroker.unsubscribe_me([
      this_module_filter(),
      height_filter(awaited_height)
    ])
  end

  defp this_module_filter() do
    %EventBroker.Filters.SourceModule{module: __MODULE__}
  end

  defp height_filter(height) do
    %__MODULE__.HeightFilter{height: height}
  end
end
