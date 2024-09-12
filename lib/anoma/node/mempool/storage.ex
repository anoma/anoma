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
    :mnesia.create_table(__MODULE__.Blocks, attributes: [:round, :block])
    :mnesia.create_table(__MODULE__.RM)
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

  def handle_call({:read, {height, key}}, from, state) do
    IO.puts("==============STORAGE READ REACHED=============")

    if height <= state.uncommitted_height + 1 do
      # relies on this being a reverse-ordered list
      IO.puts("==============SEARCH READ IN HISTORY=============")

      res =
        case Map.get(state.uncommitted_updates, key) do
          nil ->
            IO.puts("==============GET READ FROM STORAGE=============")
            tx1 = fn -> :mnesia.read(__MODULE__.Updates, key) end

            {:atomic, [{__MODULE__.Updates, _key, height_upds}]} =
              :mnesia.transaction(tx1)

            height = height_upds |> Enum.find(fn a -> a <= height end)

            tx2 = fn -> :mnesia.read(__MODULE__.Values, {height, key}) end

            {:atomic, [{__MODULE__.Values, {_height, _key}, res}]} =
              :mnesia.transaction(tx2)

            res

          res ->
            IO.puts("==============GET READ FROM UNCOMMITTED=============")
            height = res |> Enum.find(fn a -> a <= height end)
            Map.get(state.uncommitted, {height, key})
        end

      IO.puts("============READ RESULT #{inspect(res)}===========")
      {:reply, {:ok, res}, state}
    else
      IO.puts("==============READ IN THE FUTURE=============")
      block_spawn(height, fn -> blocking_read(height, key, from) end)
      {:noreply, state}
    end
  end

  def abwrite(flag, {height, key}, value, state) do
    key_old_updates = Map.get(state.uncommitted_updates, key, [])
    key_new_updates = [height | key_old_updates]
    new_updates = Map.put(state.uncommitted_updates, key, key_new_updates)

    {new_kv, value} =
      case flag do
        :append ->
          old_set_value =
            Map.get(state.uncommitted_updates, key, MapSet.new())

          new_set_value = MapSet.put(old_set_value, value)

          new_kv =
            Map.put_new(state.uncommitted, {height, key}, new_set_value)

          {new_kv, new_set_value}

        :write ->
          new_kv = Map.put_new(state.uncommitted, {height, key}, value)
          {new_kv, value}
      end

    write_event =
      EventBroker.Event.new_with_body(%__MODULE__.WriteEvent{
        height: height,
        key: key,
        value: value
      })

    EventBroker.event(write_event)

    %__MODULE__{
      state
      | uncommitted: new_kv,
        uncommitted_height: height,
        uncommitted_updates: new_updates
    }
  end

  def handle_call({:write, {height, key}, value}, from, state) do
    IO.puts("==============STORAGE WRITE REACHED=============")

    unless height == state.uncommitted_height + 1 do
      block_spawn(height - 1, fn ->
        blocking_write(height, key, value, from)
      end)

      {:noreply, state}
    else
      IO.puts("=================storage write directly=============")
      new_state = abwrite(:write, {height, key}, value, state)

      IO.puts(
        "=================REACHES END OF WRITING Storage return :ok============"
      )

      {:reply, :ok, new_state}
    end
  end

  def handle_call({:append, {height, key}, value}, from, state) do
    unless height == state.uncommitted_height + 1 do
      block_spawn(height - 1, fn ->
        blocking_write(height, key, value, from)
      end)

      {:noreply, state}
    else
      new_state = abwrite(:append, {height, key}, value, state)

      {:reply, :ok, new_state}
    end
  end

  def block_spawn(height, call) do
    {:ok, pid} =
      Task.start(call)

    EventBroker.subscribe(pid, [
      this_module_filter(),
      height_filter(height)
    ])
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

  def read({height, key}) do
    GenServer.call(__MODULE__, {:read, {height, key}}, :infinity)
  end

  def write({height, key}, value) do
    GenServer.call(__MODULE__, {:write, {height, key}, value})
  end

  def commit(block_round, writes) do
    GenServer.call(__MODULE__, {:commit, block_round, writes})
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
        GenServer.reply(from, write({height, key}, value))
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
