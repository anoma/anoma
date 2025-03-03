defmodule Anoma.Node.Transaction.Storage do
  @moduledoc """
  I am the Storage Engine.

  I represent a timestamped table-backed key-value store. I provide
  the API to read and write at specific heights and keep track of what the
  last timestamp is.

  Any time before the next timestamp or at the timestamp itself is
  considered the past. Anyting else is considered the future.

  The semantics for reads and writes are as follows:

  #### Writing

  If an actor wants to write at time T which is structly larger than the
  last timestamp I keep track of, they need to wait until my latest
  timestamp becomes T-1. In other words, you only write when your time
  comes.

  If an actor writes at time T when my timestamp is T-1, then they can
  freely write whatever they need and set my latest timestamp to T.

  #### Reading

  If an actor reads at time T which is strictly larger than the last
  timestamp I have, they need to wait until my latest timestamp becomes T.
  In other words to read in the future, you need the future to come.

  If and actor reads key K at time T which is less than or equal to my last
  timestamp, then there are two cases.

  - K was never written to have any value.
  - K was written to have some values at times T1 < T2 ... < Tn

  In the former case, I return `:absent`. In the latter case, I return the
  value of K at time Tn.

  ### Public API

  I provide the following public functionality:

  - `read/2`
  - `write/2`
  - `append/2`
  - `add/2`
  - `commit/3`
  """

  alias Anoma.Node
  alias Anoma.Node.Registry
  alias Anoma.Node.Tables
  alias Anoma.Node.Events

  require Node.Event

  use EventBroker.DefFilter
  use GenServer
  use TypedStruct

  ############################################################
  #                       Types                              #
  ############################################################

  @typedoc """
  Type of the arguments the storage genserver expects
  """
  @type args_t ::
          [
            node_id: String.t(),
            uncommitted_height: non_neg_integer()
          ]
          | [node_id: String.t()]

  @typedoc """
  I am the type of an event write.
  """
  @type event_write :: {[binary()], binary()}

  @typedoc """
  I am the type of the key to be stored.
  """
  @type bare_key() :: list(String.t())

  @typedoc """
  I am the type of the key at a specific timestamp.
  """
  @type qualified_key() :: {integer(), bare_key()}

  @typedoc """
  I am a type of writing options.
  """
  @type write_opts() :: :append | :write | :add

  ############################################################
  #                         State                            #
  ############################################################

  typedstruct enforce: true do
    @typedoc """
    I am the type of a Storage Engine.

    I store all in-progress information for matching keys and values to
    specific timestamps.

    ### Fields

    - `:node_id` - The ID of the Node to which a Storage instantiation is
                   bound.

    - `:uncommitted` - The map of keys at a specific height to its value.
                       Default: %{}
    - `:uncommitted_height` - The latest timestamp of Storage.
                              Default: 0
    - `:uncommitted_updates` - The map mapping a key to a list of all
                               timestamps at which it was updated. Reverse
                               ordered.
                               Default: %{}
    """

    field(:node_id, String.t())
    field(:uncommitted, %{qualified_key() => term()}, default: %{})
    # the most recent height written.
    # starts at 0 because nothing has been written.
    field(:uncommitted_height, non_neg_integer(), default: 0)
    # reverse-ordered list of heights at which a key was updated.
    field(:uncommitted_updates, %{bare_key() => list(integer())},
      default: %{}
    )
  end

  deffilter HeightFilter, height: non_neg_integer() do
    %EventBroker.Event{body: %Node.Event{body: %{height: ^height}}} -> true
    _ -> false
  end

  ############################################################
  #                    Genserver Helpers                     #
  ############################################################

  @doc """
  I am the start_link function of the Storage Engine.

  I register the enfine with supplied node ID provided by the arguments and
  check that the uncommitted height has been supplied.
  """

  @spec start_link() :: GenServer.on_start()
  @spec start_link(args_t) :: GenServer.on_start()
  def start_link(args \\ []) do
    args = Keyword.validate!(args, [:node_id, :uncommitted_height])
    name = Registry.via(args[:node_id], __MODULE__)
    GenServer.start_link(__MODULE__, args, name: name)
  end

  @doc """
  I am the initialization function for the Storage Engine.

  From the specified arguments, I get the node ID, and the uncommitted height.

  Afterwards I launch the Storage engine with given arguments.
  """

  @impl true
  def init(args) do
    Process.set_label(__MODULE__)

    args =
      args
      |> Keyword.validate!([
        :node_id,
        uncommitted_height: 0
      ])

    # initialize the tables in the mnesia backend
    case Tables.initialize_tables_for_node(args[:node_id]) do
      {:ok, _} ->
        state = struct(__MODULE__, Enum.into(args, %{}))

        {:ok, state}

      {:error, :failed_to_initialize_tables} ->
        {:stop, :failed_to_initialize_tables}
    end
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @doc """
  I am the Storage read function.

  I provide functionality to read a specific key at a specific height T.

  If the height provided is higher than the height of the Storage, I block
  the actor using me and wait for the height to become the one in the
  Storage, then read the key at T.

  To do that, I see whether the key has been updated in the state. If so, I
  get the value of that key at the most recent height it was updated, i.e.
  at the value closest to T.

  If not, then it might have been committed in the past and I do the same
  procedure yet in the corresponding mnesia tables.

  If nothing is found, I return `:absent`
  """

  @spec read(String.t(), {non_neg_integer(), bare_key()}) :: :absent | any()
  def read(node_id, {height, key}) do
    GenServer.call(
      Registry.via(node_id, __MODULE__),
      {:read, {height, key}},
      :infinity
    )
  end

  @doc """
  I am the Storage add function.

  I provide functionality to write and append at the same height. I am
  provided a map of things to write and things to append. I write the list
  of key-values and append the list of key-sets provided all at the same
  height.

  To check my semantics, see `write/2` and `append/2`.
  """

  @spec add(
          String.t(),
          {non_neg_integer(),
           %{
             write: list({bare_key(), any()}),
             append: list({bare_key(), MapSet.t()})
           }}
        ) :: term()
  def add(node_id, args = {_height, %{write: _writes, append: _appends}}) do
    GenServer.call(
      Registry.via(node_id, __MODULE__),
      {:add, args},
      :infinity
    )
  end

  @doc """
  I am the Storage write function.

  I am given a node ID alongside a height T and a list of key-value pairs.

  If T is larger than the uncommitted height plus one, I block the caller
  to wait until T is exactly one above the current height in the storage.
  That is, I block the caller until it is their time to write.

  When that time comes, I go through the list of key-values, add the height
  to the list which records all height at which those keys have been
  updated, then map the tuple of {T, key} to the new value and store
  it in the state.

  Once that is done, I sent a write event specifying what has been written
  and at white height, while setting the uncommitted height to T.
  """

  @spec write(String.t(), {non_neg_integer(), list({bare_key(), any()})}) ::
          :ok
  def write(node_id, {height, kvlist}) do
    GenServer.call(
      Registry.via(node_id, __MODULE__),
      {:write, {height, kvlist}},
      :infinity
    )
  end

  @doc """
  I am the Storage append function.

  I am a Storage-level abstraction for rewriting set values by appending
  new values to them.

  I am given a node ID alongside a height T and a list of key-value pairs
  where all values are expected to be sets.

  If T is larger than the uncommitted height plus one, I block the caller
  to wait until T is exactly one above the current height in the storage.
  That is, I block the caller until it is their time to write.

  When that time comes, I first read the most recent values of the keys
  supplied. If none are, I produce an empty set. Afterwards, I go through
  the list of key-values, add the height to the list which records all
  height at which those keys have been updated, then map the tuple of
  {T, key} to the union of their most recent value and the new value,
  therefore appendin to the set.

  Once that is done, I sent a write event specifying what has been written
  and at white height, while setting the uncommitted height to T.
  """

  @spec append(
          String.t(),
          {non_neg_integer(), list({bare_key(), MapSet.t()})}
        ) ::
          :ok
  def append(node_id, {height, kvlist}) do
    GenServer.call(
      Registry.via(node_id, __MODULE__),
      {:append, {height, kvlist}},
      :infinity
    )
  end

  @doc """
  I am the Storage commit function.

  I am called when an actor with Mempool functionality decides that a block
  round has been complitted, prompting the commitment of the in-progress
  storage to appropriate table and the creation of a block with supplied
  content.
  """

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

  @impl true
  def terminate(_reason, state) do
    {:ok, state}
  end

  @doc """
  I am the Storage function for getting current time.

  My main use is to coordinate latest-time reads from the user's side and
  particularly to aid read-only transactions. Note that evidently there is
  a possibility of clock desynchronization after the call. Hence it should
  only be used for approximate-time getting such as require for RO
  transactions.
  """

  @spec current_time(String.t()) :: non_neg_integer()
  def current_time(node_id) do
    GenServer.call(
      Registry.via(node_id, __MODULE__),
      :current_time
    )
  end

  ############################################################
  #                      Public Filters                      #
  ############################################################

  @doc """
  I am the height filter.

  Given a height, I provide a filter for for messages of a particular
  height.
  """
  @spec height_filter(non_neg_integer()) :: HeightFilter.t()
  def height_filter(height) do
    %__MODULE__.HeightFilter{height: height}
  end

  ############################################################
  #                       User Calling API                   #
  ############################################################

  @doc """
  I am a block table name function.

  Given a Node ID, I produce the name of the appropriate block table
  connected to it.
  """

  @spec blocks_table(String.t()) :: atom()
  def blocks_table(node_id) do
    Tables.table_blocks(node_id)
  end

  @doc """
  I am a values table name function.

  Given a Node ID, I produce the name of the appropriate values table
  connected to it.
  """
  @spec values_table(String.t()) :: atom()
  def values_table(node_id) do
    Tables.table_values(node_id)
  end

  @doc """
  I am an updates table name function.

  Given a Node ID, I produce the name of the appropriate updates table
  connected to it.
  """
  @spec updates_table(String.t()) :: atom()
  def updates_table(node_id) do
    Tables.table_updates(node_id)
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  @impl true
  def handle_call({:commit, round, writes, _}, _from, state) do
    handle_commit(round, writes, state)
    {:reply, :ok, state}
  end

  def handle_call({:read, {0, _key}}, _from, state) do
    {:reply, :absent, state}
  end

  def handle_call({:read, {height, key}}, from, state) do
    handle_read({height, key}, from, state)
  end

  def handle_call(:current_time, _from, state) do
    {:reply, state.uncommitted_height, state}
  end

  def handle_call({write_opt, {height, args}}, from, state)
      when write_opt in [:write, :append, :add] do
    handle_write(write_opt, {height, args}, from, state)
  end

  def handle_call(_msg, _from, state) do
    {:reply, :ok, state}
  end

  @impl true
  def handle_cast(_msg, state) do
    {:noreply, state}
  end

  @impl true
  def handle_info(_info, state) do
    {:noreply, state}
  end

  ############################################################
  #                 Genserver Implementation                 #
  ############################################################

  @spec handle_commit(
          non_neg_integer(),
          list(Anoma.Node.Transaction.Mempool.Tx.t()),
          t()
        ) :: t()
  defp handle_commit(round, writes, state = %__MODULE__{}) do
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

    %__MODULE__{
      state
      | uncommitted: %{},
        uncommitted_updates: %{},
        uncommitted_height: state.uncommitted_height
    }
  end

  @spec handle_read({non_neg_integer(), any()}, GenServer.from(), t()) ::
          {:noreply, t()} | {:reply, any(), t()}
  defp handle_read({height, key}, from, state) do
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

  @spec handle_write(
          write_opts(),
          {non_neg_integer(), any()},
          GenServer.from(),
          t()
        ) :: {:noreply, t()} | {:reply, :ok, t()}
  defp handle_write(write_opt, {height, args}, from, state) do
    unless height == state.uncommitted_height + 1 do
      node_id = state.node_id

      block_spawn(
        height - 1,
        fn ->
          blocking_write(node_id, height, args, from)
        end,
        node_id
      )

      {:noreply, state}
    else
      {new_state, event_writes} = abwrite(write_opt, {height, args}, state)

      Events.write_event(height, event_writes, state.node_id, __MODULE__)

      {:reply, :ok, new_state}
    end
  end

  ############################################################
  #                      Private Filters                     #
  ############################################################

  defp this_module_filter() do
    %EventBroker.Filters.SourceModule{module: __MODULE__}
  end

  ############################################################
  #                      ABWrite Helpers                     #
  ############################################################

  # todo: should exclude same key being overwritten at same height
  @spec abwrite(
          write_opts(),
          {non_neg_integer(), list(any()) | %{write: list(), append: list()}},
          t()
        ) ::
          {t(), list()}
  defp abwrite(:add, {height, %{write: writes, append: appends}}, state) do
    {new_state, updates1} = abwrite(:write, {height, writes}, state)
    {final_state, updates2} = abwrite(:append, {height, appends}, new_state)

    {final_state, updates1 ++ updates2}
  end

  defp abwrite(flag, {height, kvlist}, state) do
    for {key, value} <- kvlist,
        reduce: {%__MODULE__{state | uncommitted_height: height}, []} do
      {state_acc, list} ->
        new_key_heights = new_key_heights(key, state_acc)

        new_updates =
          Map.put(state_acc.uncommitted_updates, key, new_key_heights)

        {new_kv, event_writes_local} =
          case flag do
            :append ->
              do_append(state_acc, state, key, value, list)

            :write ->
              do_write(state_acc, key, value, kvlist)
          end

        {%__MODULE__{
           state_acc
           | uncommitted: new_kv,
             uncommitted_updates: new_updates
         }, event_writes_local}
    end
  end

  @spec do_append(t(), t(), any(), any(), list()) ::
          {any(), list()}
  defp do_append(state_acc, state = %__MODULE__{}, key, value, list) do
    height = state_acc.uncommitted_height

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

    new_set_value = MapSet.union(old_set_value, value)

    new_kv =
      Map.put(
        state_acc.uncommitted,
        {height, key},
        new_set_value
      )

    {new_kv, [{key, new_set_value} | list]}
  end

  # Write to the latest
  @spec do_write(t(), any(), any(), list()) ::
          {any(), list()}
  defp do_write(state, key, value, kvlist) do
    height = state.uncommitted_height

    new_kv =
      Map.put_new(state.uncommitted, {height, key}, value)

    {new_kv, kvlist}
  end

  @spec read_in_past(non_neg_integer(), any(), t()) ::
          :absent | :error | {:ok, term()}
  defp read_in_past(height, key, state) do
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

  @spec new_key_heights(any(), t()) :: [non_neg_integer()]
  defp new_key_heights(key, state = %__MODULE__{uncommitted_height: height}) do
    key_old_updates = Map.get(state.uncommitted_updates, key, [])

    with [latest_height | _] <- key_old_updates,
         true <- height == latest_height do
      key_old_updates
    else
      _e -> [height | key_old_updates]
    end
  end

  ############################################################
  #                    Blocking Operations                   #
  ############################################################

  defp block_spawn(height, call, node_id) do
    {:ok, pid} =
      Task.start(call)

    EventBroker.subscribe(pid, [
      Node.Event.node_filter(node_id),
      this_module_filter(),
      height_filter(height)
    ])
  end

  defp blocking_write(node_id, height, kvlist, from) do
    awaited_height = height - 1

    receive do
      %EventBroker.Event{
        body: %Node.Event{
          body: %Events.WriteEvent{height: ^awaited_height}
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

  @spec blocking_read(String.t(), non_neg_integer(), any(), GenServer.from()) ::
          :ok
  defp blocking_read(node_id, height, key, from) do
    receive do
      # if the key we care about was written at exactly the height we
      # care about, then we already have the value for free
      %EventBroker.Event{
        body: %Node.Event{
          body: %Events.WriteEvent{height: ^height, writes: writes}
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
end
