defmodule Anoma.Node.Mempool.Storage do
  @moduledoc """
  abstorage genserver
  """

  use GenServer
  use TypedStruct
  require EventBroker.Event

  alias __MODULE__

  @type bare_key() :: list(String.t())
  @type qualified_key() :: {integer(), bare_key()}

  # invariants this should enforce but does not:
  # - increasing timestamps
  #   - ideally, monotonic timestamps
  # - no writes to the "past"
  #   - uncommitted can sometimes be written out of order, though?
  # - committed/uncommitted distinction
  # - reads to "future" block rather than yielding an old value

  typedstruct enforce: true do
    field(:uncommitted, %{qualified_key() => term()}, default: %{})
    field(:uncommitted_height, integer(), default: 0)

    field(:uncommitted_updates, %{bare_key() => list(integer())},
      default: %{}
    )
  end

  typedstruct enforce: true, module: WriteEvent do
    field(:height, integer())
    field(:key, Anoma.Node.Mempool.Storage.bare_key())
    field(:value, term())
  end

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: __MODULE__)
  end

  def init(_) do
    :mnesia.create_table(__MODULE__.Values, attributes: [:key, :value])
    :mnesia.create_table(__MODULE__.Updates, attributes: [:key, :value])
    :mnesia.create_table(__MODULE__.RM)
    {:ok, %Storage{}}
  end

  def handle_call({:read, {height, key}}, from, state) do
    if height <= state.uncommitted_height do
      # relies on this being a reverse-ordered list

      res =
        case Map.get(state.uncommitted_updates, key)
             |> Enum.find(fn a -> a <= height end) do
          nil ->
            tx = fn ->
              height =
                :mnesia.read(__MODULE__.Updates, key)
                |> Enum.find(fn a -> a <= height end)

              :mnesia.read(__MODULE__.Values, {height, key})
            end

            {:ok, res} = :mnesia.tx(tx)
            res

          res ->
            res
        end

      {:reply, Map.get(state.uncommitted, {res, key}), state}
    else
      {:ok, pid} = Task.start(fn -> blocking_read(height, key, from) end)

      EventBroker.subscribe(pid, [
        this_module_filter(),
        height_filter(height)
      ])

      {:noreply, state}
    end
  end

  def handle_call(:commit, _from, state) do
    mnesia_tx = fn ->
      for {key, value} <- state.uncommitted do
        :mnesia.write(__MODULE__.Values, key, value)
      end

      for {key, value} <- state.uncommitted_updates do
        old_updates = :mnesia.read(__MODULE__.Updates, key)
        new_updates = value ++ old_updates
        :mnesia.write(__MODULE__.Updates, key, new_updates)
      end
    end

    :mnesia.transaction(mnesia_tx)
    {:noreply, %__MODULE__{uncommitted_height: state.uncommitted_height}}
  end

  def handle_call(_msg, _from, state) do
    {:reply, :ok, state}
  end

  def handle_call({:write, {height, key}, value}, from, state) do
    unless height == state.uncommitted_height + 1 do
      {:ok, pid} =
        Task.start(fn -> blocking_write(height, key, value, from) end)

      EventBroker.subscribe(pid, [
        this_module_filter(),
        height_filter(height - 1)
      ])

      {:noreply, state}
    else
      key_old_updates = Map.get(state.uncommitted_updates, key, [])
      key_new_updates = [height | key_old_updates]
      new_updates = Map.put(state.uncommitted_updates, key, key_new_updates)
      new_kv = Map.put_new(state.uncommitted, {height, key}, value)

      new_state = %{
        state
        | uncommitted: new_kv,
          uncommitted_height: height,
          uncommitted_updates: new_updates
      }

      write_event =
        EventBroker.Event.new_with_body(%__MODULE__.WriteEvent{
          height: height,
          key: key,
          value: value
        })

      EventBroker.event(write_event)

      {:reply, :ok, new_state}
    end
  end

  def handle_call({:append, {height, key}, value}, from, state) do
    unless height == state.uncommitted_height + 1 do
      {:ok, pid} =
        Task.start(fn -> blocking_write(height, key, value, from) end)

      EventBroker.subscribe(pid, [
        this_module_filter(),
        height_filter(height - 1)
      ])

      {:noreply, state}
    else
      key_old_updates = Map.get(state.uncommitted_updates, key, [])
      key_new_updates = [height | key_old_updates]
      new_updates = Map.put(state.uncommitted_updates, key, key_new_updates)
      old_set_value = Map.get(state.uncommitted_updates, key, MapSet.new())
      new_set_value = MapSet.put(old_set_value, value)
      new_kv = Map.put_new(state.uncommitted, {height, key}, new_set_value)

      new_state = %{
        state
        | uncommitted: new_kv,
          uncommitted_height: height,
          uncommitted_updates: new_updates
      }

      write_event =
        EventBroker.Event.new_with_body(%__MODULE__.WriteEvent{
          height: height,
          key: key,
          value: value
        })

      EventBroker.event(write_event)

      {:noreply, new_state}
    end
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

  def write({height, key}, value) do
    GenServer.call(__MODULE__, {:write, {height, key}, value})
  end

  def commit() do
    GenServer.call(__MODULE__, :commit)
  end

  defp blocking_read(height, key, from) do
    receive do
      # if the key we care about was written at exactly the height we
      # care about, then we already have the value for free
      %EventBroker.Event{
        body: %__MODULE__.WriteEvent{height: ^height, key: ^key, value: value}
      } ->
        GenServer.reply(from, value)

      # else read the value with a call
      %EventBroker.Event{
        body: %__MODULE__.WriteEvent{height: ^height}
      } ->
        GenServer.reply(from, read({height, key}))

      _ ->
        IO.puts("this should be unreachable")
    end

    EventBroker.unsubscribe_me([
      this_module_filter(),
      height_filter(height)
    ])
  end

  def blocking_write(height, key, value, from) do
    awaited_height = height - 1

    receive do
      %EventBroker.Event{
        body: %__MODULE__.WriteEvent{height: ^awaited_height}
      } ->
        write({height, key}, value)
    end

    EventBroker.unsubscribe_me([
      this_module_filter(),
      height_filter(height)
    ])
  end

  defp this_module_filter() do
    %EventBroker.Filters.SourceModule{module: __MODULE__}
  end

  defp height_filter(height) do
    %__MODULE__.HeightFilter{height: height}
  end
end
