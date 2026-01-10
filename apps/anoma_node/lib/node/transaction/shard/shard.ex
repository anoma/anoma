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

  ############################################################
  #                         State                            #
  ############################################################

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

    field(
      :kv,
      %{required(key()) => %{required(height()) => details()}},
      default: %{}
    )

    field(
      :watermarks,
      %{required(key()) => %{read: height(), write: height()}},
      default: %{}
    )

    field(
      :pending_reads,
      %{required(key()) => %{required(height()) => list(GenServer.from())}},
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
  defp handle_read(key, height_req, from, state) do
    # --- Validation ---
    key_height_map = Map.get(state.kv, key, %{})

    details_at_req =
      Map.get(key_height_map, height_req, @default_details)

    # If we hit the default, we fail
    # Unify check_pending_reads
    cond do
      # 1. Check Read Reservation
      details_at_req.read_reserved_count == 0 ->
        {:reply, {:error, :read_not_reserved}, state}

      # 2. Attempt Resolution
      true ->
        key_watermarks =
          Map.get(state.watermarks, key, @initial_watermarks)

        resolution_result =
          resolve_read_value(height_req, key_height_map, key_watermarks)

        case resolution_result do
          # Includes {:ok, :absent} or {:ok, {:ok, val}}
          {:ok, value_or_absent} ->
            # Resolve succeeded, release reservation and reply
            updated_details = %{
              details_at_req
              | read_reserved_count: details_at_req.read_reserved_count - 1
            }

            new_key_height_map =
              Map.put(key_height_map, height_req, updated_details)

            new_kv = Map.put(state.kv, key, new_key_height_map)
            new_state = %{state | kv: new_kv}

            # Map internal {:ok, :absent} to just :absent for the caller
            {:reply, value_or_absent, new_state}

          block_reason
          when block_reason in [
                 :blocked_by_watermark,
                 :blocked_by_write_reservation
               ] ->
            # Queue the read
            pending_for_key = Map.get(state.pending_reads, key, %{})
            current_pending_list = Map.get(pending_for_key, height_req, [])
            updated_pending_list = [from | current_pending_list]

            updated_pending_for_key =
              Map.put(pending_for_key, height_req, updated_pending_list)

            new_pending_reads =
              Map.put(state.pending_reads, key, updated_pending_for_key)

            {:noreply, %{state | pending_reads: new_pending_reads}}
        end
    end
  end

  @spec handle_write_watermark_advanced(key(), height(), t()) :: t()
  defp handle_write_watermark_advanced(key, h_write, state) do
    current_key_watermarks =
      Map.get(state.watermarks, key, @initial_watermarks)

    # Write watermark should only advance
    new_write_wm = max(h_write, current_key_watermarks.write)

    if new_write_wm > current_key_watermarks.write do
      updated_watermarks = %{current_key_watermarks | write: new_write_wm}
      new_watermarks_map = Map.put(state.watermarks, key, updated_watermarks)

      state_after_wm_update = %__MODULE__{
        state
        | watermarks: new_watermarks_map
      }

      # Check pending reads based ONLY on the new write watermark
      check_pending_reads(key, state_after_wm_update)
    else
      # Watermark did not advance for this key
      state
    end
  end

  @spec handle_read_watermark_advanced(key(), height(), t()) :: t()
  defp handle_read_watermark_advanced(key, h_read, state) do
    current_key_watermarks =
      Map.get(state.watermarks, key, @initial_watermarks)

    # Read watermark should only advance
    new_read_wm = max(h_read, current_key_watermarks.read)

    if new_read_wm > current_key_watermarks.read do
      updated_watermarks = %{current_key_watermarks | read: new_read_wm}
      new_watermarks_map = Map.put(state.watermarks, key, updated_watermarks)

      state_after_wm_update = %__MODULE__{
        state
        | watermarks: new_watermarks_map
      }

      # Perform Garbage Collection based ONLY on the new read watermark
      state_after_gc = gc_key(key, new_read_wm, state_after_wm_update)

      state_after_gc
    else
      # Watermark did not advance for this key
      state
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
          if not is_nil(details.value) do
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
  @spec check_pending_reads(key(), __MODULE__.t()) :: __MODULE__.t()
  defp check_pending_reads(key, state) do
    pending_for_key = Map.get(state.pending_reads, key, %{})
    key_watermarks = Map.get(state.watermarks, key, @initial_watermarks)

    # Iterate through pending heights {height_req => from_list}
    {new_pending_for_key, final_state_after_key_reduction} =
      Enum.reduce(pending_for_key, {%{}, state}, fn {height_req, from_list},
                                                    {acc_pending_map,
                                                     acc_state_outer} ->
        # Re-fetch key_height_map for each height_req as it's modified by inner reduce
        current_key_height_map_outer = Map.get(acc_state_outer.kv, key, %{})

        resolution_result =
          resolve_read_value(
            height_req,
            current_key_height_map_outer,
            key_watermarks
          )

        case resolution_result do
          block_reason
          when block_reason in [
                 :blocked_by_watermark,
                 :blocked_by_write_reservation
               ] ->
            # Still blocked, keep pending
            {Map.put(acc_pending_map, height_req, from_list), acc_state_outer}

          {:ok, value_or_absent} ->
            # Process each requester in the list, decrementing reservations one by one
            final_acc_state_inner =
              Enum.reduce(from_list, acc_state_outer, fn requester_from,
                                                         acc_state_inner ->
                # Get freshest details for THIS requester, as it might have been updated by previous one in list
                current_key_height_map_inner =
                  Map.get(acc_state_inner.kv, key, %{})

                details_at_req_height =
                  Map.get(
                    current_key_height_map_inner,
                    height_req,
                    @default_details
                  )

                if details_at_req_height.read_reserved_count > 0 do
                  GenServer.reply(requester_from, value_or_absent)

                  updated_details = %{
                    details_at_req_height
                    | read_reserved_count:
                        details_at_req_height.read_reserved_count - 1
                  }

                  new_key_height_map_inner =
                    Map.put(
                      current_key_height_map_inner,
                      height_req,
                      updated_details
                    )

                  new_kv_inner =
                    Map.put(acc_state_inner.kv, key, new_key_height_map_inner)

                  %{acc_state_inner | kv: new_kv_inner}
                else
                  # No reservation left, reply with error
                  GenServer.reply(
                    requester_from,
                    {:error, :read_not_reserved}
                  )

                  # State (kv) doesn't change as no reservation was consumed
                  acc_state_inner
                end
              end)

            # All requesters for this height_req processed, remove from pending map
            {acc_pending_map, final_acc_state_inner}
        end
      end)

    new_pending_reads =
      if map_size(new_pending_for_key) > 0 do
        Map.put(
          final_state_after_key_reduction.pending_reads,
          key,
          new_pending_for_key
        )
      else
        Map.delete(final_state_after_key_reduction.pending_reads, key)
      end

    %{final_state_after_key_reduction | pending_reads: new_pending_reads}
  end

  # Finds essential heights to keep below a given target height.
  # Returns a MapSet containing:
  # - The height of the latest committed value strictly below target_height.
  # - The heights of all write reservations between that value and target_height.
  @spec find_essential_heights_below(height(), map()) :: MapSet.t(height())
  defp find_essential_heights_below(target_height, key_height_map) do
    # 1. Find the highest height h_val < target_height with a committed value
    maybe_max_h_val =
      key_height_map
      |> Enum.filter(fn {h, details} ->
        h < target_height and not is_nil(details.value)
      end)
      |> Enum.max_by(fn {h, _} -> h end, fn -> nil end)

    #  Do the common processing then do the case
    case maybe_max_h_val do
      nil ->
        # No committed value below target_height. Find write reservations below target_height.
        write_reservation_heights_below =
          key_height_map
          |> Enum.filter(fn {h, details} ->
            h < target_height and details.write_reserved?
          end)
          # Keep only the heights
          |> Enum.map(fn {h, _} -> h end)

        MapSet.new(write_reservation_heights_below)

      {h_val, _details} ->
        # 2. Find all heights h_wr with write reservations between h_val and target_height
        write_reservation_heights_between =
          key_height_map
          |> Enum.filter(fn {h, details} ->
            h > h_val and h < target_height and details.write_reserved?
          end)
          # Keep only the heights
          |> Enum.map(fn {h, _} -> h end)

        # 3. Combine h_val and the intermediate write reservation heights
        MapSet.new([h_val | write_reservation_heights_between])
    end
  end

  # I am the garbage collection helper function.

  # I perform garbage collection for a specific key based on the read watermark.
  # I remove entries older than the watermark unless they are essential for resolving
  # reads at or past the watermark height or at heights with active read reservations.
  @spec gc_key(key(), height(), __MODULE__.t()) :: __MODULE__.t()
  defp gc_key(key, read_watermark, state) do
    case Map.get(state.kv, key) do
      nil ->
        # Key not present, nothing to GC
        state

      key_height_map ->
        # 1. Identify heights with active read reservations
        read_reservation_heights =
          for {h, details} <- key_height_map,
              details.read_reserved_count > 0,
              into: MapSet.new(),
              do: h

        # 2. Determine essential heights to keep below the read watermark
        essential_below_watermark =
          find_essential_heights_below(read_watermark, key_height_map)

        # 3. Determine essential heights to keep below each active read reservation
        essential_below_reservations =
          read_reservation_heights
          |> Enum.map(&find_essential_heights_below(&1, key_height_map))
          |> Enum.reduce(MapSet.new(), &MapSet.union/2)

        # 4. Combine all heights that MUST be kept:
        #    - Heights holding read reservations themselves.
        #    - Essential heights supporting the watermark.
        #    - Essential heights supporting each reservation.
        all_essential_heights_below_watermark =
          read_reservation_heights
          |> MapSet.union(essential_below_watermark)
          |> MapSet.union(essential_below_reservations)

        # 5. Filter the map: Keep entries >= watermark OR in the essential set below watermark
        new_key_height_map =
          Enum.filter(key_height_map, fn {h, _details} ->
            # Keep if at or above watermark OR essential below
            h >= read_watermark or
              MapSet.member?(all_essential_heights_below_watermark, h)
          end)
          |> Map.new()

        # Note there might be a subtle issue with the semantics of
        # forgetting something that hasn't existed vs has once existed
        # but has been tombstoned

        # 6. Update state
        if map_size(new_key_height_map) > 0 do
          new_kv = Map.put(state.kv, key, new_key_height_map)
          %{state | kv: new_kv}
        else
          # If GC removed all entries for the key, remove the key itself
          new_kv = Map.delete(state.kv, key)
          %{state | kv: new_kv}
        end
    end
  end

  # I am the helper function to attempt resolving a read request.

  # I check if a read for `key` at `height_req` can be resolved based on the
  # current `key_height_map` and `key_watermarks`.
  # I return:
  # - `{:ok, :absent}` if resolvable and no value exists below `height_req`.
  # - `{:ok, {:ok, value}}` if resolvable and a value exists.
  # - `:blocked_by_watermark` if `height_req` is above the write watermark.
  # - `:blocked_by_write_reservation` if the latest entry below `height_req` holds a write reservation.
  @spec resolve_read_value(height(), map(), map()) ::
          {:ok, :absent | {:ok, value()}}
          | :blocked_by_watermark
          | :blocked_by_write_reservation
  defp resolve_read_value(height_req, key_height_map, key_watermarks) do
    if height_req > key_watermarks.write + 1 do
      :blocked_by_watermark
    else
      # 2. Find the latest entry below height_req that has EITHER a value OR a write reservation.
      # This represents the most recent operation determining the state relevant to the read.
      maybe_relevant_entry =
        key_height_map
        |> Enum.filter(fn {h, details} ->
          h < height_req and
            (not is_nil(details.value) or details.write_reserved?)
        end)
        |> Enum.max_by(fn {h, _details} -> h end, fn -> nil end)

      case maybe_relevant_entry do
        # 3. No relevant entry found below height_req (implies initial state or empty)
        nil ->
          # If no entry with a value or reservation exists below height_req, the result is absent.
          {:ok, :absent}

        # 4. Relevant entry found, check its state
        {_h, details} ->
          cond do
            # If the latest relevant entry has a value (is committed), resolve the read.
            not is_nil(details.value) ->
              {:ok, {:ok, details.value}}

            # If the latest relevant entry holds a write reservation, block the read.
            details.write_reserved? ->
              :blocked_by_write_reservation

            # Should be unreachable.
            true ->
              Logger.error(
                "Shard: Unreachable state in resolve_read_value for key height map: #{inspect(key_height_map)}, height_req: #{height_req}"
              )

              # Treat as absent if we somehow reach here
              {:ok, :absent}
          end
      end
    end
  end

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

  # pattern match this, remove the :read_write.
  defp check_watermarks(height, :read, %{read: mark}) when height <= mark do
    {:error, :reserving_read_under_read_watermark}
  end

  defp check_watermarks(_height, _type, _key_watermarks), do: :ok

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
