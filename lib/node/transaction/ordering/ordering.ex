defmodule Anoma.Node.Transaction.Ordering do
  @moduledoc """
  I am the Ordering Engine.

  I act as a mediator between Workers and Shards. In particular, Workers
  working on a transaction may ask to read and write information. However,
  they do not know when to do it, they only know the ID of the transaction
  they work on. Moreover, they do not know where the key they are
  interested in is stored, i.e. on which shard.

  I process such requests, keeping them waiting until consensus provides
  some ordering to a transaction in question. Once they do, I pair a
  transaction ID with its timestamp and forward queries to appropriate
  Shards.

  ### Public API

  I provide the following public functonality:

  - `read/2`
  - `write/2`
  - `order/2`
  - `reserve/3`
  - `commit/4`
  """

  alias __MODULE__
  alias Anoma.Node
  alias Anoma.Node.Registry
  alias Anoma.Tables
  alias Anoma.Node.Transaction.Backends
  alias Anoma.Node.Transaction.Shard
  alias Anoma.Node.Transaction.Shard.Supervisor

  require Node.Event

  use EventBroker.DefFilter
  use GenServer
  use TypedStruct

  ############################################################
  #                       Types                              #
  ############################################################

  @typedoc """
  Type of the arguments the ordering genserver expects
  """
  @type args_t ::
          [
            node_id: String.t(),
            next_height: non_neg_integer()
          ]
          | [node_id: String.t()]

  @typedoc """
  I am the type of possible ordering operations
  """
  @type flag :: :read | :write

  @typedoc """
  I am the type of reservations
  """
  @type reservations :: %{:read => MapSet.t(), :write => MapSet.t()}

  @typedoc """
  I am a request that can be enqued
  """
  @type request :: {flag(), GenServer.from(), list({any(), any()}) | any()}
  ############################################################
  #                         State                            #
  ############################################################

  typedstruct enforce: true do
    @typedoc """
    I am the type of the Ordering Enigine.

    I contain the Node for which the Ordering is launched, the upcoming
    hight as well as a map from transaction IDs to their global order.

    ### Fields

    - `:node_id` - The ID of the Node to which an Ordering instantiation is
                   bound.
    - `:next_height` - The height that the next ordered transaction
                       candidate will get.
                       Default: 1
    - `:tx_id_to_height` - A map from an ID of a transaction candidate to
                           its order.
    - `:requests` - A map from the ID of a transaction candidate to its
                    worker address with the request to read or write.
                   Default:  %{}
    - `:shard_addresses` - The addresses of the shards containing keys.
                           Default: %{}
    - `:reservations` - The map from IDs to lists of read and write
                        reservations to be forwarded to shards.
                        Default: %{}
    - `:block_key_order` - The map from keys to a list of height ordered
                           from most recent. Used to advance watermarks
                           Default: %{}
    """
    field(:node_id, String.t())
    field(:next_height, integer(), default: 1)
    field(:tx_id_to_height, %{binary() => integer()}, default: %{})

    field(:requests, %{binary() => request()}, default: %{})

    field(:shard_addresses, %{any() => pid()}, default: %{})

    field(:reservations, %{any() => reservations()}, default: %{})

    field(:block_key_order, %{any() => list({flag(), non_neg_integer()})},
      default: %{}
    )
  end

  @doc """
  I am the start_link function for the Ordering Engine.

  I register the engine with supplied node ID provided by the arguments.
  """

  @spec start_link(args_t()) :: GenServer.on_start()
  def start_link(args \\ []) do
    name = Registry.via(args[:node_id], __MODULE__)
    GenServer.start_link(__MODULE__, args, name: name)
  end

  ############################################################
  #                    Genserver Helpers                     #
  ############################################################

  @doc """
  I am the initialization function for the Ordering Engine.

  From the specified arguments, I get the Node ID as well as the info
  regarding the next height Ordering should be started with. I also
  subscribe to the completion events to finalize shard interactions.
  """

  @impl true
  def init(args) do
    Process.set_label(__MODULE__)

    args = Keyword.validate!(args, [:node_id, next_height: 1])

    state = struct(Ordering, Enum.into(args, %{}))

    EventBroker.subscribe_me([
      Node.Event.node_filter(args[:node_id]),
      %Backends.Events.CompleteFilter{}
    ])

    {:ok, state}
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @doc """
  I am the Ordering read function.

  I receive a Node ID and an {id, key} tuple. There are two states possible
  when Ordering processes my request. Either:

  - The id has been assigned an order.
  - The id has not been assigned an order.

  If the former is true, I send the request to read the key at the
  specified height to the appropriate shard. That is, we ask the shard to
  read the most recent value assigned to the key from the point of view of
  the transaction candidate. See `Shard.read/3`.

  If the latter is true, I leave the caller blocked until the id has been
  assigned a value, i.e. until a corresponding event gets received.
  """

  @spec read(String.t(), {binary(), any()}) :: any()
  def read(node_id, {id, key}) do
    GenServer.call(
      Registry.via(node_id, __MODULE__),
      {:read, {id, key}},
      :infinity
    )
  end

  @doc """
  I am the Ordering write function.

  I receive a Node ID and an {id, kvlist} tuple. There are two states
  possible when Ordering processes my request. Either:

  - The id has been assigned an order.
  - The id has not been assigned an order.

  If the former is true, I send the request to write the key-value list at
  the specified height to the appropriate shards by separating each element
  in the list to its own request. See `Shard.write/4`

  If the latter is true, I leave the caller blocked until the id has been
  assigned a value, i.e. until a corresponding event gets received.
  """

  @spec write(String.t(), {binary(), list({any(), any()})}) ::
          :ok | {:error, :fail_to_write}
  def write(node_id, {id, kvlist}) do
    GenServer.call(
      Registry.via(node_id, __MODULE__),
      {:write, {id, kvlist}},
      :infinity
    )
  end

  @doc """
  I am the Ordering commit function. I handle checkpointing by committing
  blocks and shard states to tables.

  Given a set of IDs for a block alongside with appropriate writes, I make
  sure that no reservations are to be completed, then ask each shard to
  backup state.

  Finally, I create the block with the transaction completion information.
  """
  @spec commit(String.t(), any(), any(), MapSet.t()) :: :ok
  def commit(node_id, block_round, writes, ids) do
    GenServer.call(
      Registry.via(node_id, __MODULE__),
      {:commit, block_round, writes, ids},
      :infinity
    )
  end

  @doc """
  I am the Ordering order function.

  Given a Node ID and a list of transaction IDs, I percieve the latter as a
  partial ordering of transactions. Afterwards, I assign them a global
  ordering by adding the next height stored in the Ordering Engine to the
  respective ordering inside a list.

  Afterwards, I send an event specifying that a particular ID has indeed
  received an order.

  WARNING :: NO Read Only Transaction ID's should
  """
  @spec order(String.t(), [binary()]) :: :ok
  def order(node_id, txs) do
    GenServer.cast(Registry.via(node_id, __MODULE__), {:order, txs})
  end

  @doc """
  I am the Ordering reserve function.

  Given an ID of a transaction, there are two possible states:

  - The id has been assigned an order.
  - The id has not been assigned an order.

  If the former is true, I forward the reservations to the appropriate
  shards at the appropriate orders. Otherwise, I store the reservation
  requests in my state to forward them to shards once heights have been
  assigned.
  """
  @spec reserve(String.t(), binary(), %{
          :read => MapSet.t(),
          :write => MapSet.t()
        }) ::
          :ok
  def reserve(node_id, tx_id, reservations) do
    GenServer.cast(
      Registry.via(node_id, __MODULE__),
      {:reserve, tx_id, reservations}
    )
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  @impl true
  def handle_call({:write, args}, from, state) do
    {:noreply, handle_write(args, from, state)}
  end

  def handle_call({:read, {tx_id, key}}, from, state) do
    {:noreply, handle_read({tx_id, key}, from, state)}
  end

  def handle_call({:commit, round, writes, ids}, from, state) do
    handle_commit(round, writes, ids, from, state)
    {:noreply, state}
  end

  def handle_call(_msg, _from, state) do
    {:reply, :ok, state}
  end

  @impl true
  def handle_cast({:order, tx_id_list}, state) do
    {:noreply, handle_order(tx_id_list, state)}
  end

  @impl true
  def handle_cast({:reserve, tx_id, reservations}, state) do
    {:noreply, handle_reserve(tx_id, reservations, state)}
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end

  @impl true
  def handle_info(
        %EventBroker.Event{
          body: %Node.Event{body: %Backends.Events.CompleteEvent{tx_id: id}}
        },
        state
      ) do
    {:noreply, handle_complete_event(id, state)}
  end

  @impl true
  def handle_info(_info, state) do
    {:noreply, state}
  end

  ############################################################
  #                 Genserver Implementation                 #
  ############################################################

  @spec handle_write(
          {binary(), list({any(), any()})},
          GenServer.from(),
          t()
        ) :: t()
  defp handle_write({tx_id, list}, from, state) do
    keys = Enum.map(list, &elem(&1, 0)) |> Enum.into(MapSet.new())

    state_w_shards = ensure_all_started(state, keys)

    process_request({:write, from, list}, tx_id, state_w_shards)
  end

  @spec handle_read({binary(), any()}, GenServer.from(), t()) :: t()
  defp handle_read({tx_id, key}, from, state) do
    state_w_shards = ensure_started(state, key)
    process_request({:read, from, key}, tx_id, state_w_shards)
  end

  @spec handle_commit(
          non_neg_integer,
          any(),
          MapSet.t(binary()),
          GenServer.from(),
          t()
        ) :: :ok
  defp handle_commit(round, writes, ids, from, state) do
    if Enum.any?(ids, &Map.has_key?(state.reservations, &1)) do
      # if there is some reservation being held, completion message lags
      # temporary until execution rework
      Task.start(fn ->
        GenServer.reply(
          from,
          Ordering.commit(state.node_id, round, writes, ids)
        )
      end)
    else
      for {key, pid} <- state.shard_addresses do
        # if all candidates in block completed:
        # advance watermarks to most recent height for ro txs
        Shard.advance_watermark(pid, key, state.next_height - 1)
        # backup state
        Shard.backup_state(pid)
      end

      noun_writes = Enum.map(writes, &Noun.Nounable.to_noun/1)

      {:atomic, _} =
        :mnesia.transaction(fn ->
          :mnesia.write(
            {Tables.table_blocks(state.node_id), ["anoma", "block", round],
             noun_writes}
          )
        end)

      GenServer.reply(from, :ok)
    end
  end

  @spec handle_order(list(binary()), t()) :: t()
  defp handle_order(tx_id_list, state) do
    {map, next_order, keymap} =
      for tx_id <- tx_id_list,
          reduce: {state.tx_id_to_height, state.next_height, %{}} do
        {map, order, map_of_keyheights} ->
          reservations = Map.get(state.reservations, tx_id)

          # Reserve the shards
          reserve_keys(reservations, order, state.shard_addresses)
          # Creates Key ⟶ [{flag, order}]

          # This must be ordered correctly?
          final_keymap =
            reserve_order_mapping(reservations, order)
            |> Map.merge(map_of_keyheights, fn _k, v1, v2 -> v2 ++ v1 end)

          with {:ok, request} <- Map.fetch(state.requests, tx_id) do
            # if any requests were made by workers, forward them to shards
            fire_request(request, order, tx_id, state)
          end

          {Map.put(map, tx_id, order), order + 1, final_keymap}
      end

    # advance write watermarks of keys as much as possible
    for {key, list} <- keymap do
      pid = Map.fetch!(state.shard_addresses, key)
      handle_advance_watermark(list, key, pid)
    end

    %__MODULE__{
      state
      | tx_id_to_height: map,
        next_height: next_order,
        block_key_order: keymap
    }
  end

  @spec handle_reserve(binary(), %{:read => list(), :write => list()}, t()) ::
          t()
  defp handle_reserve(tx_id, res, state) do
    # update reservation list, and reserve shard addresses
    new_state =
      %__MODULE__{
        state
        | reservations: Map.put(state.reservations, tx_id, res)
      }
      |> ensure_all_started(MapSet.union(res.read, res.write))

    unless Enum.empty?(res.write) do
      # if usual transaction do nothing
      new_state
    else
      # else handle read only transaction
      # store the tx at that height without advancing the order
      # this guaranees referential transparency
      %__MODULE__{
        new_state
        | tx_id_to_height:
            Map.put(new_state.tx_id_to_height, tx_id, new_state.next_height)
      }
    end
  end

  @spec handle_complete_event(binary(), t()) :: t()
  defp handle_complete_event(id, state) do
    reservations = Map.get(state.reservations, id)
    time = Map.fetch!(state.tx_id_to_height, id)
    keys = MapSet.union(reservations.read, reservations.write)

    keymap =
      for key <- keys, reduce: state.block_key_order do
        map ->
          pid = Map.fetch!(state.shard_addresses, key)

          # advance relevant write watermarks if non-ro-tx completed
          if Enum.empty?(reservations.write) do
            map
          else
            case Map.fetch!(map, key) do
              [{_flag, ^time}] ->
                # if no further reservations, delete the key
                Map.delete(map, key)

              [{_flag, ^time} | tl] ->
                # if further reservations, advance watermarks
                rest = handle_advance_watermark(tl, key, pid)

                Map.put(map, key, rest)

              list ->
                # if out of order, a candidate errores, remove the heights
                Map.put(
                  map,
                  key,
                  Enum.reject(list, fn {_type, x} ->
                    x == time
                  end)
                )
            end
          end
      end

    # Only remove the writes, we allow reading, so we can read in the
    # past, if the TX has finalized, the height has passed, so we
    # should be able to read safely in the past that is finalized, we need to fix the shard logic

    # TODO :: Remove in next pass
    # Unreserve reservations that may be out
    for {:write, keys} <- Map.fetch!(state.reservations, id) do
      for key <- keys do
        state.shard_addresses
        |> Map.fetch!(key)
        |> Shard.unreserve(key, time)
      end
    end

    %__MODULE__{
      state
      | requests: Map.delete(state.requests, id),
        reservations: Map.delete(state.reservations, id),
        block_key_order: keymap
    }
  end

  ############################################################
  #                           Helpers                        #
  ############################################################

  @spec ensure_all_started(t(), Enum.t()) :: t()
  defp ensure_all_started(state, keys) do
    Enum.reduce(keys, state, fn key, state -> ensure_started(state, key) end)
  end

  @spec ensure_started(t(), any()) :: t()
  defp ensure_started(state, key) do
    case Map.fetch(state.shard_addresses, key) do
      {:ok, _pid} ->
        # if we already know the shard PID do nothing
        state

      :error ->
        # otherwise ask Shard Supervisor to start a new one
        # revealing PID or creating a new Shard
        case Supervisor.start_shard(state.node_id, key) do
          {:ok, pid} ->
            %__MODULE__{
              state
              | shard_addresses: Map.put(state.shard_addresses, key, pid)
            }

          {:error, {:already_started, pid}} ->
            %__MODULE__{
              state
              | shard_addresses: Map.put(state.shard_addresses, key, pid)
            }
        end
    end
  end

  @spec process_request(request(), binary(), t()) :: t()
  defp process_request(request, tx_id, state) do
    case Map.fetch(state.tx_id_to_height, tx_id) do
      {:ok, height} ->
        # if we know the height, process the request
        fire_request(request, height, tx_id, state)

        state

      :error ->
        # otherwise store the request
        %__MODULE__{state | requests: Map.put(state.requests, tx_id, request)}
    end
  end

  @spec reserve_keys(reservations(), non_neg_integer(), %{any() => pid()}) ::
          :ok
  defp reserve_keys(%{write: write_keys}, order, addresses) do
    Enum.each(write_keys, fn key ->
      Map.fetch!(addresses, key)
      |> Shard.reserve(key, order)
    end)
  end

  @spec reserve_order_mapping(reservations(), non_neg_integer()) :: %{
          any() => list()
        }
  defp reserve_order_mapping(%{read: read_keys, write: write_keys}, order) do
    # Only record reads when writes are absent (merge takes the 2nd)
    Map.new(read_keys, &{&1, [{:read, order}]})
    |> Map.merge(Map.new(write_keys, &{&1, [{:write, order}]}))
  end

  @spec handle_advance_watermark([{flag(), any()}], any(), pid()) :: any()
  defp handle_advance_watermark([], _key, _pid), do: []

  defp handle_advance_watermark(block_list, key, pid) do
    Enum.reduce_while(
      block_list,
      tl(block_list),
      fn
        {:read, order}, [] ->
          # if no further keys, advance to the last read of the block
          Shard.advance_watermark(pid, key, order - 1)
          {:halt, []}

        {:read, _order}, [_hd | new_tl] ->
          # otherwise search for the next write (or last read)
          {:cont, new_tl}

        {:write, order}, acc ->
          # if we hit a write, advance watermark to it directly
          Shard.advance_watermark(pid, key, order - 1)
          {:halt, [{:write, order} | acc]}
      end
    )
  end

  @spec fire_request(request(), non_neg_integer(), binary(), t()) ::
          {:ok, pid()} | :ok
  defp fire_request({:read, from, key}, height, _, %__MODULE__{
         shard_addresses: addresses
       }) do
    Task.start(fn ->
      resp = Shard.read(Map.fetch!(addresses, key), key, height)
      GenServer.reply(from, resp)
    end)
  end

  defp fire_request({:write, from, keys}, height, tx_id, %__MODULE__{
         shard_addresses: addresses,
         reservations: reservations
       }) do
    with %{write: possible_writes} <- Map.fetch!(reservations, tx_id),
         write_keys = MapSet.new(keys, fn {k, _} -> k end),
         true <- MapSet.subset?(write_keys, possible_writes) do
      Task.start(fn ->
        Enum.each(keys, fn {key, value} ->
          Map.fetch!(addresses, key)
          |> Shard.write(key, value, height)
        end)

        GenServer.reply(from, :ok)
      end)
    else
      _ -> GenServer.reply(from, {:error, :fail_to_write})
    end
  end
end
