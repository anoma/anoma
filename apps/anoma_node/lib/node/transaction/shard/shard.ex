# Simplify data type by turning the SOA data type into it's own data
# structure with it's own API
defmodule Anoma.Node.Transaction.Shard do
  @moduledoc """
  I am the Shard module.

  I manage a partition of the distributed key-value store, handling requests
  for reserving slots, reading, and writing specific keys at specific heights.
  I maintain versioned state
  for read resolution and garbage collection based on dual watermarks.

  ### Public API

  I provide the following public functionality:

  - `start_link/1`
  - `reserve/4`
  - `read/3`
  - `write/4`
  - `unreserve/4`
  - `backup_state/1`
  - `advance_read_watermark/3`
  - `advance_write_watermark/3`
  - `debug_get_state/1`

  ### Key Concepts

  - **Height:** A transaction-specific identifier used for versioning.
  - **KV State:** A map storing key -> height -> entry_details.
  - **Reservations:** Independent read and write reservations associated with a `{key, height}`.
  - **Watermarks:** Per-key dual watermarks (`:read`, `:write`) control GC and read resolution respectively.
  - **Synchronous Reads:** Read requests (`read/3`) block the caller
      until resolved. Resolution may be delayed internally if blocked
      by watermarks or preceding write reservations. Read completion
      releases the specific read reservation.
  """

  alias Anoma.Node.Registry

  require Logger

  use GenServer
  use TypedStruct

  @default_details %{
    value: nil,
    read_reserved_count: 0,
    write_reserved?: false
  }
  @initial_watermarks %{read: 0, write: 0}

  ############################################################
  #                       Types                              #
  ############################################################

  @typedoc "The key in the key-value store."
  @type key :: [binary()]

  @typedoc "The height associated with an operation."
  # Allows 0 for initial state
  @type height :: integer()

  @typedoc "The value stored for a key at a height."
  @type value :: any()

  @typedoc "The capabilities requested or held by a reservation."
  @type capabilities :: :read | :write | :read_write

  @typedoc "Stores the details for a specific {key, height}."
  @type details :: %{
          # The actual value, nil if not written yet
          value: value() | nil,
          # Count of active read reservations
          read_reserved_count: non_neg_integer(),
          # True if write-reserved, false otherwise
          write_reserved?: boolean()
        }

  @type startup_options ::
          {:node_id, String.t()}
          | {:inital_kv, %{key() => %{height() => details()}}}
          | {:id, atom()}

  @type write_reserved :: %{read_reserved_count: non_neg_integer()}
  @type filled :: %{value: value()}

  @typedoc "I represent the value of a Cell, I can be in 3 different states:

  1. I am empty
  2. I could be filled with a value
  3. I could have a write reservation out"
  @type cell_slot :: filled() | :empty | write_reserved()

  ############################################################
  #                         State                            #
  ############################################################

  typedstruct enforce: true, module: Cell do
    @typedoc """
    I represent a cell within a shard
    """
    field(:pending, nil | list(GenServer.from()))
    field(:read_reserved, non_neg_integer())
    field(:cell, Anoma.Node.Transaction.Shard.cell_slot())
  end

  typedstruct enforce: true do
    @typedoc """
    I am the state of the Shard GenServer.

    ### Fields
    - `:id` - The identifier for this shard.
    - `:node_id` - The ID of the node this shard belongs to.
    - `:kv` - The core key-value store: `key => height => details`.
    - `:watermarks` - Per-key watermarks: `key => %{read: height, write: height}`.
    - `:pending_reads` - Reads blocked by a watermark or write reservation: `key => height => GenServer.from()`.
    """
    field(:id, atom())
    field(:node_id, String.t())
    field(:cells, %{key() => Cell.t()}, default: %{})
    field(:kv, %{key() => %{height() => details()}}, default: %{})

    field(:watermarks, %{key() => %{read: height(), write: height()}},
      default: %{}
    )

    field(:pending_reads, %{key() => %{height() => list(GenServer.from())}},
      default: %{}
    )
  end

  ############################################################
  #                    Genserver Helpers                     #
  ############################################################

  @doc """
  I am the start_link function for the Shard module.

  I start and link a Shard process, register it using the provided `id`,
  and initialize its KV state based on `initial_kv` options.
  """
  @spec start_link(list(startup_options())) :: GenServer.on_start()
  def start_link(args) do
    args = Keyword.validate!(args, [:id, :node_id, initial_kv: %{}])
    name = Registry.via(args[:node_id], __MODULE__, args[:id])
    GenServer.start_link(__MODULE__, args, name: name)
  end

  @impl true
  def init(args) do
    Process.set_label(__MODULE__)

    kv =
      Map.new(args[:initial_kv], fn {key, value} ->
        {key, %{0 => %{@default_details | value: value}}}
      end)

    watermarks =
      Map.new(args[:initial_kv], fn {key, _} -> {key, @initial_watermarks} end)

    {id, node_id} = {args[:id], args[:node_id]}

    state =
      %__MODULE__{id: id, node_id: node_id, kv: kv, watermarks: watermarks}

    {:ok, state}
  end

  ############################################################
  #                    Public RPC API                      #
  ############################################################

  @doc """
  I am the reserve function for the Shard module. Use me to reserve a
  read or write at a specific key at a specific height.

  Reservations exist to inform the KV store that a value will
  be read or written at a specific height at some point in the future.
  If I know that an empty entry will be written to, then an immediate read
  will have to wait until the write occurs. If I know that some entry will
  be read from, then I know I must keep around immediately preceding committed values at
  least until the read is completed.

  I request a reservation on a specific key at a given height.
  Capabilities can be `:read`, `:write`, or `:read_write`.
  I return `:ok` on success, or an error tuple.
  """
  # Might be a call? Need to know if we can reserve it
  @spec reserve(GenServer.server(), key(), height(), capabilities()) ::
          :ok
          | {:error,
             :reserving_write_under_write_watermark
             | :reserving_read_under_read_watermark
             | :slot_occupied_by_value}
  def reserve(shard_pid, key, height, type) do
    # Todo: Timeout?
    # Remove timeout
    GenServer.call(shard_pid, {:reserve, key, height, type})
  end

  @doc """
  I am the read function for the Shard module.

  I perform a synchronous read request for a key at a specific height.
  I require a prior `reserve` call with `:read` or `:read_write` capability for this `{key, height}`.
  The caller blocks until the read can be resolved (potentially waiting for watermarks)
  and receives the result directly.
  Returns `{:ok, value}`, `:absent`, or an error tuple.
  """
  @spec read(GenServer.server(), key(), height()) ::
          {:ok, value()}
          | :absent
          | {:error, :read_not_reserved}
  def read(shard_pid, key, height) do
    GenServer.call(shard_pid, {:read, key, height}, :infinity)
  end

  @doc """
  I am the write function for the Shard module.

  I write a value for a key at a specific height, requiring a prior `reserve` call
  with `:write` or `:read_write` capability for this `{key, height}`.
  I return `:ok` on success, or an error tuple.
  """
  @spec write(GenServer.server(), key(), value(), height()) :: :ok
  def write(shard_pid, key, value, height) do
    GenServer.cast(shard_pid, {:write, key, value, height})
  end

  @doc """
  I release a specific type of reservation (:read or :write) for a given key at a given height.

  This is an asynchronous operation primarily used for rollbacks of failed transactions.
  """
  @spec unreserve(GenServer.server(), key(), height(), :read | :write) :: :ok
  def unreserve(shard_pid, key, height, type) do
    GenServer.cast(shard_pid, {:unreserve, key, height, type})
  end

  @doc """
  I trigger a backup of the shard's internal state to Mnesia.

  This is a synchronous operation.
  """
  @spec backup_state(GenServer.server()) :: :ok | {:error, any()}
  def backup_state(shard_pid) do
    # No Infinity, unsure if should be a call as we may care about
    # blocking due to 2 phase commit
    GenServer.call(shard_pid, :backup_state)
  end

  @doc """
  I advance the read watermark for a given key.

  This is an asynchronous operation.
  """
  @spec advance_read_watermark(GenServer.server(), key(), height()) :: :ok
  def advance_read_watermark(shard_pid, key, h_read) do
    GenServer.cast(shard_pid, {:read_watermark_advanced, key, h_read})
  end

  @doc """
  I advance the write watermark for a given key.

  This is an asynchronous operation.
  """
  @spec advance_write_watermark(GenServer.server(), key(), height()) :: :ok
  def advance_write_watermark(shard_pid, key, h_write) do
    GenServer.cast(shard_pid, {:write_watermark_advanced, key, h_write})
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  @impl true
  def handle_call({:reserve, key, height, type}, _from, state) do
    {response, state} = handle_reserve(key, height, type, state)
    {:reply, response, state}
  end

  def handle_call({:read, key, height_req}, from, state) do
    handle_read(key, height_req, from, state)
  end

  def handle_call(:backup_state, _from, state) do
    handle_backup_state(state)
    {:reply, :ok, state}
  end

  @impl true
  def handle_cast({:write, key, value, height}, state) do
    {:noreply, handle_write(key, value, height, state)}
  end

  def handle_cast({:write_watermark_advanced, key, h_write}, state) do
    {:noreply, handle_write_watermark_advanced(key, h_write, state)}
  end

  def handle_cast({:read_watermark_advanced, key, h_read}, state) do
    {:noreply, handle_read_watermark_advanced(key, h_read, state)}
  end

  def handle_cast({:unreserve, key, height, type}, state) do
    {:noreply, handle_unreserve(key, height, type, state)}
  end

  ############################################################
  #                 Genserver Implementation                 #
  ############################################################

  @spec handle_reserve(key(), height(), capabilities(), t()) ::
          {:ok | {:error, atom()}, t()}
  defp handle_reserve(key, height, cap, state) do
    key_watermarks = Map.get(state.watermarks, key, @initial_watermarks)

    with :ok <- check_watermarks(height, cap, key_watermarks),
         :ok <- get_details(state.kv, key, height) |> reserve_detail_err(cap) do
      kv = update_details(state.kv, key, height, &reserve_detail(&1, cap))
      {:ok, %__MODULE__{state | kv: kv}}
    else
      err -> {err, state}
    end
  end

  @spec handle_write(key(), value(), height(), t()) :: t()
  defp handle_write(key, value, height, state = %{kv: kv}) do
    ori_details = get_details(kv, key, height)

    if !ori_details.write_reserved? do
      state
    else
      new_kv =
        replace_details(kv, key, height, &unreserve_detail(&1, :write, value))

      check_pending_reads(key, %__MODULE__{state | kv: new_kv})
    end
  end

  @spec handle_read(key(), height(), GenServer.from(), t()) ::
          {:reply, {:ok, value()} | :absent | {:error, atom()}, t()}
          | {:noreply, t()}
  defp handle_read(key, height, from, state = %{kv: kv, pending_reads: pend}) do
    key_height_map = Map.get(kv, key, %{})

    details = get_details(kv, key, height)

    # Unify check_pending_reads
    cond do
      details.read_reserved_count == 0 ->
        {:reply, {:error, :read_not_reserved}, state}

      # 2. Attempt Resolution
      true ->
        key_watermarks =
          Map.get(state.watermarks, key, @initial_watermarks)

        resolution =
          resolve_read_value(height, key_height_map, key_watermarks)

        cond do
          resolution in [:blocked_by_watermark, :blocked_by_write_reservation] ->
            new_pending_reads =
              Map.update(pend, key, %{height => [from]}, fn pending ->
                Map.update(pending, height, [from], &[from | &1])
              end)

            {:noreply, %{state | pending_reads: new_pending_reads}}

          true ->
            new_kv =
              replace_details(kv, key, height, &unreserve_detail(&1, :read))

            {:reply, resolution, %__MODULE__{state | kv: new_kv}}
        end
    end
  end

  @spec handle_write_watermark_advanced(key(), height(), t()) :: t()
  defp handle_write_watermark_advanced(key, h_write, state) do
    cur_watermark = Map.get(state.watermarks, key, @initial_watermarks)
    new_watermark = advance_watermarks(key, h_write, :write, state.watermarks)

    if cur_watermark == new_watermark do
      state
    else
      check_pending_reads(key, %__MODULE__{state | watermarks: new_watermark})
    end
  end

  @spec handle_read_watermark_advanced(key(), height(), t()) :: t()
  defp handle_read_watermark_advanced(key, h_read, state) do
    cur_watermark = Map.get(state.watermarks, key, @initial_watermarks)
    new_watermark = advance_watermarks(key, h_read, :read, state.watermarks)

    if cur_watermark == new_watermark do
      state
    else
      read_key = new_watermark[key].read
      gc_key(key, read_key, %{state | watermarks: new_watermark})
    end
  end

  @spec handle_unreserve(key(), height(), :read | :write, t()) :: t()
  defp handle_unreserve(key, height, type, state) do
    new_kv =
      replace_details(state.kv, key, height, &unreserve_detail(&1, type))

    new_state = %__MODULE__{state | kv: new_kv}

    ori_details = get_details(state.kv, key, height)
    new_details = get_details(new_kv, key, height)

    cond do
      ori_details == new_details ->
        new_state

      type == :write and ori_details.write_reserved? ->
        check_pending_reads(key, new_state)

      true ->
        new_state
    end
  end

  @spec handle_backup_state(t()) :: :ok
  defp handle_backup_state(%__MODULE__{id: id, node_id: node_id, kv: kv}) do
    backup_table = Anoma.Node.Tables.table_shard_backups(node_id)

    mnesia_tx = fn ->
      # Overwrite for same height.
      Enum.each(kv, fn {key, height_map} ->
        Enum.each(height_map, fn {height, details} ->
          # Only backup entries with a committed value
          if details.value do
            record_key = {id, key, height}
            :mnesia.write({backup_table, record_key, details.value})
          end
        end)
      end)
    end

    with {:aborted, reason} <- :mnesia.transaction(mnesia_tx) do
      Logger.error("Shard #{inspect(id)} backup failed: #{inspect(reason)}")
    end

    :ok
  end

  ############################################################
  #                           Helpers                        #
  ############################################################

  # I am the helper function to check pending reads after a watermark update, write, or unreserve.

  # This isn't a check, we are sending messages to people waiting on reads

  # I check all pending reads for a given key.
  # If a read becomes resolvable, I calculate the result, reply directly to the waiting
  # caller using `GenServer.reply/2`, release the corresponding read reservation, and
  # remove the request from the pending map.
  @spec check_pending_reads(key(), t()) :: t()
  defp check_pending_reads(key, state) do
    key_watermarks = Map.get(state.watermarks, key, @initial_watermarks)

    key_map = Map.get(state.kv, key, %{})

    reading_for_pendings =
      state.pending_reads
      |> Map.get(key, %{})
      |> Enum.map(fn {height, pendings} ->
        {height, pendings,
         resolve_read_value(height, key_map, key_watermarks)}
      end)

    {still_pending, can_resolve} =
      Enum.split_with(reading_for_pendings, fn {_, _, resolved} ->
        resolved in [:blocked_by_watermark, :blocked_by_write_reservation]
      end)

    pending_for_key = Map.new(still_pending, fn {h, p, _} -> {h, p} end)

    final_pending_map =
      if Enum.empty?(pending_for_key) do
        Map.delete(state.pending_reads, key)
      else
        Map.put(state.pending_reads, key, pending_for_key)
      end

    new_details =
      Map.new(can_resolve, fn {height, pendings, value} ->
        details = get_details(state.kv, key, height)
        new_count = max(0, details.read_reserved_count - length(pendings))

        {can_send, no_send} =
          Enum.split(pendings, details.read_reserved_count)

        Enum.each(can_send, &GenServer.reply(&1, value))
        Enum.each(no_send, &GenServer.reply(&1, {:error, :read_not_reserved}))

        {height, %{details | read_reserved_count: new_count}}
      end)

    %__MODULE__{
      state
      | pending_reads: final_pending_map,
        kv: Map.replace_lazy(state.kv, key, &Map.merge(&1, new_details))
    }
  end

  # Finds essential heights to keep below a given target height.
  # Returns a MapSet containing:
  # - The height of the latest committed value strictly below target_height.
  # - The heights of all write reservations between that value and target_height.
  @spec find_essential_heights_below(height(), map()) :: MapSet.t(height())
  defp find_essential_heights_below(target_height, key_height_map) do
    # Sort relevant values backwards to grab most relevant items first
    sorted_lower_heights =
      key_height_map
      |> Enum.filter(fn {h, _} -> h < target_height end)
      |> Enum.sort(:desc)

    reserved_heights =
      sorted_lower_heights
      |> Enum.filter(fn {_, details} -> details.write_reserved? end)
      |> MapSet.new(fn {h, _} -> h end)

    case Enum.find(sorted_lower_heights, fn {_, det} -> det.value end) do
      nil ->
        reserved_heights

      {h_val, _details} ->
        reserved_heights
        |> MapSet.filter(fn h -> h > h_val end)
        |> MapSet.put(h_val)
    end
  end

  # I am the garbage collection helper function.

  # I perform garbage collection for a specific key based on the read watermark.
  # I remove entries older than the watermark unless they are essential for resolving
  # reads at or past the watermark height or at heights with active read reservations.
  # Note there might be a subtle issue with the semantics of
  # forgetting something that hasn't existed vs has once existed
  # but has been tombstoned
  @spec gc_key(key(), height(), t()) :: t()
  defp gc_key(key, read_watermark, state) do
    height_map = Map.get(state.kv, key, %{})

    reserved_heights =
      height_map
      |> Enum.filter(fn {h, _} -> h <= read_watermark end)
      |> Enum.filter(fn {_, %{read_reserved_count: c}} -> c > 0 end)
      |> MapSet.new(fn {h, _} -> h end)

    essential_heights =
      reserved_heights
      |> MapSet.put(read_watermark)
      |> Enum.map(&find_essential_heights_below(&1, height_map))
      |> Enum.reduce(MapSet.new(), &MapSet.union/2)

    heights_left_below = MapSet.union(reserved_heights, essential_heights)

    all_keys_left =
      Map.filter(height_map, fn {h, _} ->
        h >= read_watermark or MapSet.member?(heights_left_below, h)
      end)

    kv =
      if Enum.empty?(all_keys_left) do
        Map.delete(state.kv, key)
      else
        Map.put(state.kv, key, all_keys_left)
      end

    %{state | kv: kv}
  end

  # I check if a read for `key` at `height_req` can be resolved based on the
  # current `key_height_map` and `key_watermarks`.
  @spec resolve_read_value(height(), map(), map()) ::
          :absent
          | {:ok, value()}
          | :blocked_by_watermark
          | :blocked_by_write_reservation
  defp resolve_read_value(height, _, %{write: w}) when height > w + 1 do
    :blocked_by_watermark
  end

  defp resolve_read_value(height_req, key_height_map, _) do
    # This represents the most recent operation relevant to the read.
    relevant_entry =
      key_height_map
      |> Enum.filter(fn {h, det} ->
        h < height_req && (det.value || det.write_reserved?)
      end)
      |> Enum.max(fn -> nil end)

    case relevant_entry do
      nil -> :absent
      {_, %{write_reserved?: true}} -> :blocked_by_write_reservation
      {_, %{value: val}} -> {:ok, val}
    end
  end

  ############################################################
  #                       Cell Operations                    #
  ############################################################

  ############################################################
  #                      Helpers Watermarks                  #
  ############################################################

  # Checks if a reservation request conflicts with existing watermarks.
  @spec check_watermarks(height(), capabilities(), map()) ::
          :ok | {:error, atom()}
  defp check_watermarks(height, type, %{write: key})
       when type in [:write, :read_write] and height <= key do
    {:error, :reserving_write_under_write_watermark}
  end

  defp check_watermarks(height, :read, %{read: mark}) when height <= mark do
    {:error, :reserving_read_under_read_watermark}
  end

  defp check_watermarks(_height, _type, _key_watermarks), do: :ok

  @spec advance_watermarks(key(), height(), :read | :write, map()) :: map()
  def advance_watermarks(key, height, type, watermark) do
    update = fn x -> max(height, x) end
    initial = Map.replace_lazy(@initial_watermarks, type, update)

    watermark
    |> Map.update(key, initial, fn watermark ->
      Map.update(watermark, type, update.(@initial_watermarks), update)
    end)
  end

  ############################################################
  #                       Helpers Details                    #
  ############################################################

  @spec get_details(map(), key(), height()) :: details()
  defp get_details(kv, key, height) do
    kv
    |> Map.get(key, %{})
    |> Map.get(height, @default_details)
  end

  ######################################
  #              Iterators             #
  ######################################

  @spec replace_details(map(), key(), height(), (details() -> details())) ::
          map()
  defp replace_details(kv, key, height, function) do
    Map.replace_lazy(kv, key, fn key_height ->
      Map.replace_lazy(key_height, height, function)
    end)
  end

  # Note I run on the default details as well!
  defp update_details(kv, key, height, function) do
    default_value = function.(@default_details)

    Map.update(kv, key, %{height => default_value}, fn key_height ->
      Map.update(key_height, height, default_value, function)
    end)
  end

  ######################################
  #                Usage               #
  ######################################

  @spec reserve_detail(details(), capabilities()) :: details()
  defp reserve_detail(details = %{value: value}, type)
       when not is_nil(value) and type in [:write, :read_write] do
    details
  end

  defp reserve_detail(details, :write) do
    %{details | write_reserved?: true}
  end

  defp reserve_detail(details, :read) do
    %{details | read_reserved_count: details.read_reserved_count + 1}
  end

  defp reserve_detail(details, :read_write) do
    %{
      details
      | read_reserved_count: details.read_reserved_count + 1,
        write_reserved?: true
    }
  end

  @spec unreserve_detail(details(), :write | :read_write, any()) :: details()
  defp unreserve_detail(details, cap, value) do
    %{unreserve_detail(details, cap) | value: value}
  end

  @spec unreserve_detail(details(), capabilities()) :: details()
  defp unreserve_detail(details, :read) do
    count = max(0, details.read_reserved_count - 1)
    %{details | read_reserved_count: count}
  end

  defp unreserve_detail(details, :write) do
    %{details | write_reserved?: false}
  end

  @spec reserve_detail_err(details(), capabilities()) ::
          :ok | {:error, atom()}
  defp reserve_detail_err(%{value: value}, cap)
       when not is_nil(value) and cap in [:write, :read_write] do
    {:error, :slot_occupied_by_value}
  end

  defp reserve_detail_err(_, _), do: :ok
end
