defmodule Anoma.Node.Transaction.Storage do
  @moduledoc """
  abstorage genserver
  """

  alias Anoma.Node
  alias Node.Registry

  use EventBroker.DefFilter
  use GenServer
  use TypedStruct
  require Node.Event

  @type bare_key() :: list(String.t())
  @type qualified_key() :: {integer(), bare_key()}
  @typep startup_options() :: {:node_id, String.t()}

  typedstruct enforce: true do
    field(:node_id, String.t())
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

  deffilter HeightFilter, height: non_neg_integer() do
    %EventBroker.Event{body: %Node.Event{body: %{height: ^height}}} -> true
    _ -> false
  end

  @spec start_link() :: GenServer.on_start()
  @spec start_link(list(startup_options())) :: GenServer.on_start()
  def start_link(args \\ []) do
    args = Keyword.validate!(args, [:node_id])
    name = Registry.via(args[:node_id], __MODULE__)
    GenServer.start_link(__MODULE__, args, name: name)
  end

  @spec init([startup_options()]) :: {:ok, t()}
  def init(args) do
    Process.set_label(__MODULE__)

    keylist =
      args
      |> Keyword.validate!([
        :node_id,
        rocks: false
      ])

    node_id = keylist[:node_id]

    init_tables(node_id, args[:rocks])

    state = struct(__MODULE__, Enum.into(args, %{}))

    {:ok, state}
  end

  def terminate(_reason, state) do
    {:ok, state}
  end

  def handle_call({:commit, round, writes, _}, _from, state) do
    mnesia_tx = fn ->
      for {key, value} <- state.uncommitted do
        :mnesia.write({values_table(state.node_id), key, value})
      end

      for {key, value} <- state.uncommitted_updates do
        {:atomic, res} =
          fn -> :mnesia.read(updates_table(state.node_id), key) end
          |> :mnesia.transaction()

        updates_table = updates_table(state.node_id)

        new_updates =
          case res do
            [] -> value
            [{^updates_table, _key, list}] -> value ++ list
          end

        :mnesia.write({updates_table(state.node_id), key, new_updates})
      end

      :mnesia.write({blocks_table(state.node_id), round, writes})
    end

    :mnesia.transaction(mnesia_tx)

    state = %{
      state
      | uncommitted: %{},
        uncommitted_updates: %{},
        uncommitted_height: state.uncommitted_height
    }

    {:reply, :ok, state}
  end

  def handle_call({:read, {0, _key}}, _from, state) do
    {:reply, :absent, state}
  end

  def handle_call({:read, {height, key}}, from, state) do
    if height <= state.uncommitted_height do
      # relies on this being a reverse-ordered list
      result =
        read_in_past(height, key, state)

      {:reply, result, state}
    else
      node_id = state.node_id

      block_spawn(
        height,
        fn ->
          blocking_read(node_id, height, key, from)
        end,
        node_id
      )

      {:noreply, state}
    end
  end

  def handle_call({write_or_append, {height, kvlist}}, from, state)
      when write_or_append in [:write, :append] do
    unless height == state.uncommitted_height + 1 do
      node_id = state.node_id

      block_spawn(
        height - 1,
        fn ->
          blocking_write(node_id, height, kvlist, from)
        end,
        node_id
      )

      {:noreply, state}
    else
      new_state = abwrite(write_or_append, {height, kvlist}, state)

      {:reply, :ok, new_state}
    end
  end

  def handle_call(_msg, _from, state) do
    {:reply, :ok, state}
  end

  def block_spawn(height, call, node_id) do
    {:ok, pid} =
      Task.start(call)

    EventBroker.subscribe(pid, [
      Node.Event.node_filter(node_id),
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

  @spec read(String.t(), {non_neg_integer(), any()}) :: :absent | any()
  def read(node_id, {height, key}) do
    GenServer.call(
      Registry.via(node_id, __MODULE__),
      {:read, {height, key}},
      :infinity
    )
  end

  @spec write(String.t(), {non_neg_integer(), list(any())}) :: :ok
  def write(node_id, {height, kvlist}) do
    GenServer.call(
      Registry.via(node_id, __MODULE__),
      {:write, {height, kvlist}},
      :infinity
    )
  end

  @spec append(String.t(), {non_neg_integer(), list(any())}) :: :ok
  def append(node_id, {height, kvlist}) do
    GenServer.call(
      Registry.via(node_id, __MODULE__),
      {:append, {height, kvlist}},
      :infinity
    )
  end

  @spec commit(
          String.t(),
          non_neg_integer(),
          list(Anoma.Node.Transaction.Mempool.Tx.t()) | nil
        ) :: :ok
  def commit(node_id, block_round, writes) do
    GenServer.call(
      Registry.via(node_id, __MODULE__),
      {:commit, block_round, writes, self()}
    )
  end

  @spec blocking_read(String.t(), non_neg_integer(), any(), GenServer.from()) ::
          :ok
  defp blocking_read(node_id, height, key, from) do
    receive do
      # if the key we care about was written at exactly the height we
      # care about, then we already have the value for free
      %EventBroker.Event{
        body: %Node.Event{
          body: %__MODULE__.WriteEvent{height: ^height, writes: writes}
        }
      } ->
        case Enum.find(writes, fn {keywrite, _value} -> key == keywrite end) do
          # try reading in history instead
          nil ->
            GenServer.reply(from, read(node_id, {height, key}))

          # return value
          {_key, value} ->
            GenServer.reply(from, {:ok, value})
        end

      _ ->
        IO.puts("this should be unreachable")
    end

    EventBroker.unsubscribe_me([
      Node.Event.node_filter(node_id),
      this_module_filter(),
      height_filter(height)
    ])
  end

  # todo: should exclude same key being overwritten at same height
  @spec abwrite(:append | :write, {non_neg_integer(), list(any())}, t()) ::
          t()
  def abwrite(flag, {height, kvlist}, state) do
    {new_state, event_writes} =
      for {key, value} <- kvlist,
          reduce: {%__MODULE__{state | uncommitted_height: height}, kvlist} do
        {state_acc, list} ->
          key_old_updates = Map.get(state_acc.uncommitted_updates, key, [])

          key_new_updates =
            with [latest_height | _] <- key_old_updates,
                 true <- height == latest_height do
              key_old_updates
            else
              _e -> [height | key_old_updates]
            end

          new_updates =
            Map.put(state_acc.uncommitted_updates, key, key_new_updates)

          {new_kv, event_writes_local} =
            case flag do
              :append ->
                old_set_value =
                  case Map.get(state_acc.uncommitted, {height, key}) do
                    nil ->
                      case read_in_past(height, key, state) do
                        :absent -> MapSet.new()
                        {:ok, res} -> res
                      end

                    res ->
                      res
                  end

                new_set_value = MapSet.put(old_set_value, value)

                new_kv =
                  Map.put(
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
             state_acc
             | uncommitted: new_kv,
               uncommitted_updates: new_updates
           }, event_writes_local}
      end

    write_event =
      Node.Event.new_with_body(state.node_id, %__MODULE__.WriteEvent{
        height: height,
        writes: event_writes
      })

    EventBroker.event(write_event)

    new_state
  end

  @spec read_in_past(non_neg_integer(), any(), t()) ::
          :absent | :error | {:ok, term()}
  def read_in_past(height, key, state) do
    case Map.get(state.uncommitted_updates, key) do
      nil ->
        tx1 = fn -> :mnesia.read(updates_table(state.node_id), key) end

        {:atomic, tx1_result} = :mnesia.transaction(tx1)

        updates_table = updates_table(state.node_id)

        case tx1_result do
          [{^updates_table, _key, height_upds}] ->
            height = height_upds |> Enum.find(fn a -> a <= height end)
            values_table = values_table(state.node_id)

            case height do
              nil ->
                :absent

              _ ->
                tx2 = fn ->
                  :mnesia.read(values_table, {height, key})
                end

                {:atomic, [{^values_table, {_height, _key}, tx2_result}]} =
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
            Map.fetch(state.uncommitted, {update_height, key})
        end
    end
  end

  def blocking_write(node_id, height, kvlist, from) do
    awaited_height = height - 1

    receive do
      %EventBroker.Event{
        body: %Node.Event{
          body: %__MODULE__.WriteEvent{height: ^awaited_height}
        }
      } ->
        GenServer.reply(from, write(node_id, {height, kvlist}))
    end

    EventBroker.unsubscribe_me([
      Node.Event.node_filter(node_id),
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

  @spec init_tables(atom(), bool()) :: any()
  def init_tables(node_id, rocks) do
    rocks_opt = Anoma.Utility.rock_opts(rocks)

    :mnesia.create_table(
      values_table(node_id),
      rocks_opt ++ [attributes: [:key, :value]]
    )

    :mnesia.create_table(
      updates_table(node_id),
      rocks_opt ++ [attributes: [:key, :value]]
    )

    :mnesia.create_table(
      blocks_table(node_id),
      rocks_opt ++ [attributes: [:round, :block]]
    )
  end

  def blocks_table(node_id) do
    String.to_atom("#{__MODULE__.Blocks}_#{:erlang.phash2(node_id)}")
  end

  def values_table(node_id) do
    String.to_atom("#{__MODULE__.Values}_#{:erlang.phash2(node_id)}")
  end

  def updates_table(node_id) do
    String.to_atom("#{__MODULE__.Updates}_#{:erlang.phash2(node_id)}")
  end
end
