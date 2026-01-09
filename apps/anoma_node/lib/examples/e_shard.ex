defmodule Anoma.Node.Examples.EShard do
  @moduledoc """
  I contain examples on how to interact with the Shard module.
  """

  alias Anoma.Node
  alias Anoma.Node.Registry
  alias Anoma.Node.Transaction.Shard
  alias Anoma.Node.Examples.ENode

  import ExUnit.Assertions

  @doc """
  I start a node with shards "a", "b", "c", initializing "a" and "c"
  with specific values, and verify the initial state.
  """
  @spec spawn_node_with_initial_state(String.t()) :: String.t()
  def spawn_node_with_initial_state(node_id \\ Node.example_random_id()) do
    # Define the schema: keys "a", "b", "c". "a" and "c" have initial values.
    schema = [{["a"], 5}, ["b"], {["c"], 15}]
    shard_config = [strategy: :one_per_key, schema: schema]
    opts = [node_id: node_id, transaction: [shards: shard_config]]

    # Start the node
    ENode.start_node(opts)

    # Get shard PIDs
    pid_a = Registry.whereis(node_id, Shard, :a)
    pid_b = Registry.whereis(node_id, Shard, :b)
    pid_c = Registry.whereis(node_id, Shard, :c)

    assert is_pid(pid_a), "Shard 'a' PID not found."
    assert is_pid(pid_b), "Shard 'b' PID not found."
    assert is_pid(pid_c), "Shard 'c' PID not found."

    # Get initial states
    state_a = :sys.get_state(pid_a)
    state_b = :sys.get_state(pid_b)
    state_c = :sys.get_state(pid_c)

    # Verify initial states at height 0
    assert Map.get(state_a.kv, ["a"], %{})[0].value == 5,
           "Shard 'a' initial value mismatch at height 0"

    assert Map.get(state_c.kv, ["c"], %{})[0].value == 15,
           "Shard 'c' initial value mismatch at height 0"

    # Shard "b" should have no entry for key "b" at height 0
    b_key_map = Map.get(state_b.kv, ["b"], %{})

    refute Map.has_key?(b_key_map, 0),
           "Shard 'b' should not have an initial value at height 0"

    node_id
  end

  @doc """
  I start a Shard with a predefined initial state and verify that
  reading the initial state (at height 1) returns the correct values.
  """
  @spec start_and_initial_state(String.t()) :: String.t()
  def start_and_initial_state(node_id \\ Node.example_random_id()) do
    spawn_node_with_initial_state(node_id)

    shard_a_via = Registry.via(node_id, Shard, :a)
    shard_b_via = Registry.via(node_id, Shard, :b)
    shard_c_via = Registry.via(node_id, Shard, :c)

    # --- Simulate Watermark Advancement prior to acquiring reservations ---
    Shard.advance_write_watermark(shard_c_via, ["c"], 5)

    # --- Acquire Reservations First ---
    assert :ok == Shard.reserve(shard_a_via, ["a"], 1, :read)
    assert :ok == Shard.reserve(shard_b_via, ["b"], 5, :read)
    assert :ok == Shard.reserve(shard_c_via, ["c"], 4, :read)

    # --- Simulate Watermark Advancement after acquiring reservations ---
    Shard.advance_write_watermark(shard_a_via, ["a"], 1)
    Shard.advance_write_watermark(shard_b_via, ["b"], 10)

    # --- Test Reads (Now that watermarks allow immediate resolution) ---

    # Test key "a"
    assert Shard.read(shard_a_via, ["a"], 1) == {:ok, 5}

    # Test key "b"
    assert Shard.read(shard_b_via, ["b"], 5) == :absent

    # Test key "c"
    assert Shard.read(shard_c_via, ["c"], 4) == {:ok, 15}

    node_id
  end

  @doc """
  I test a scenario where a read is requested before the watermark allows,
  then the watermark advances, and the read completes.
  """
  @spec queued_read(String.t()) :: String.t()
  def queued_read(node_id \\ Node.example_random_id()) do
    spawn_node_with_initial_state(node_id)

    shard_via = Registry.via(node_id, Shard, :a)

    key = ["a"]
    height = 7

    # 1. Acquire Reservation
    assert :ok == Shard.reserve(shard_via, key, height, :read)

    # 2. Start the read in a separate task (it will block)
    read_task =
      Task.async(fn ->
        Shard.read(shard_via, key, height)
      end)

    # 2.5 Verify the read task is initially blocked
    assert Task.yield(read_task, 100) == nil,
           "Read task should be blocked before watermark advances"

    # 3. Advance the watermark *after* the read call is blocked
    Shard.advance_write_watermark(shard_via, key, height + 1)

    # 4. Wait for the result from the task (should unblock now)
    result = Task.await(read_task, 1000)

    # 5. Assert the result
    assert result == {:ok, 5}

    node_id
  end

  @doc """
  I test a variation of queued read where a write reservation is acquired and
  a write is performed *after* the read is queued but *before* the read resolves,
  affecting the read's outcome.
  """
  @spec queued_read_with_intermediate_write(String.t()) :: String.t()
  def queued_read_with_intermediate_write(node_id \\ Node.example_random_id()) do
    spawn_node_with_initial_state(node_id)

    shard_via = Registry.via(node_id, Shard, :a)

    key = ["a"]
    read_height = 7
    # Height for the intermediate write
    write_height = 5
    # Value for the intermediate write
    write_value = 10

    # 1. Acquire Read Reservation for the future read
    assert :ok == Shard.reserve(shard_via, key, read_height, :read)

    # 2. Start the read in a separate task (it will block)
    read_task =
      Task.async(fn ->
        Shard.read(shard_via, key, read_height)
      end)

    # 2.5 Verify the read task is initially blocked
    assert Task.yield(read_task, 100) == nil,
           "Read task should be blocked initially"

    # 3. Acquire Write Reservation for an intermediate height BEFORE watermark advances
    assert :ok == Shard.reserve(shard_via, key, write_height, :write)

    # 4. Advance the watermark AFTER the read call is blocked, enabling read resolution
    # WM >= read_height
    Shard.advance_write_watermark(shard_via, key, read_height + 1)

    # 5. Perform the Write AFTER watermark advanced but potentially before read task resumes
    # This write should be visible to the resolving read at height 7.
    assert :ok ==
             Shard.write(
               shard_via,
               key,
               write_value,
               write_height
             )

    # 6. Await the result from the read task (should unblock due to WM)
    result = Task.await(read_task, 1000)

    # 7. Assert the result
    # The read at height 7 should resolve to the latest write strictly below 7.
    # The write at height 5 (value 10) occurred before the read resolved.
    # The initial value at 0 is 5.
    # Therefore, the latest write < 7 is the one at height 5.
    assert result == {:ok, write_value}

    node_id
  end

  @doc """
  I test a scenario where a read is requested, but the watermark never
  advances, causing the read to time out.
  """
  @spec read_timeout(String.t()) :: String.t()
  def read_timeout(node_id) do
    spawn_node_with_initial_state(node_id)

    shard_via = Registry.via(node_id, Shard, :a)

    key = ["a"]
    height = 5

    # 1. Acquire Reservation
    assert :ok == Shard.reserve(shard_via, key, height, :read)

    # 2. Start the read in a separate task (it will block)
    read_task =
      Task.async(fn ->
        # Note: The GenServer.call within Shard.read uses :infinity,
        # so this task itself won't timeout internally. The timeout
        # comes from Task.await below.
        Shard.read(shard_via, key, height)
      end)

    # 2.5 Verify the read task is initially blocked
    assert Task.yield(read_task, 100) == nil,
           "Read task should be blocked before trying to await with timeout"

    # 3. DO NOT advance the watermark

    # 4. Await the result with a short timeout
    # We expect this to exit with reason :timeout
    try do
      Task.await(read_task, 100)
      # If await succeeds, the test fails
      flunk("Task.await should have timed out and exited, but it returned.")
    catch
      :exit, reason ->
        assert reason == :timeout or match?({:timeout, _}, reason)
    end

    if Process.alive?(read_task.pid),
      do: Task.shutdown(read_task, :brutal_kill)

    node_id
  end

  @doc """
  I test a scenario with two pending reads at different heights.
  An intermediate watermark advance unblocks only the lower-height read,
  while the higher-height read eventually times out.
  """
  @spec partial_read_unblocking_with_timeout(String.t()) :: String.t()
  def partial_read_unblocking_with_timeout(
        node_id \\ Node.example_random_id()
      ) do
    spawn_node_with_initial_state(node_id)

    shard_via = Registry.via(node_id, Shard, :a)

    key = ["a"]

    read_height_ok = 5
    read_height_timeout = 15
    watermark_height = 10

    # 1. Acquire Reservations
    assert :ok == Shard.reserve(shard_via, key, read_height_ok, :read)

    assert :ok == Shard.reserve(shard_via, key, read_height_timeout, :read)

    # 2. Start Read Tasks (both will block initially)
    read_task_ok =
      Task.async(fn ->
        Shard.read(shard_via, key, read_height_ok)
      end)

    read_task_timeout =
      Task.async(fn ->
        Shard.read(shard_via, key, read_height_timeout)
      end)

    # 2.5 Verify both tasks are initially blocked
    assert Task.yield(read_task_ok, 100) == nil,
           "Read task (ok) should be blocked initially"

    assert Task.yield(read_task_timeout, 100) == nil,
           "Read task (timeout) should be blocked initially"

    # 3. Advance Watermark partially (enough for height 5, not for 15)
    Shard.advance_write_watermark(shard_via, key, watermark_height)

    # 4. Await the read that should succeed
    result_ok = Task.await(read_task_ok, 1000)

    # Read at height 5 resolves based on latest write < 5, which is initial value 5 at height 0
    assert result_ok == {:ok, 5}

    # 5. Await the read that should time out
    try do
      Task.await(read_task_timeout, 100)

      flunk(
        "Task for height #{read_height_timeout} should have timed out, but it returned."
      )
    catch
      :exit, reason ->
        assert reason == :timeout or match?({:timeout, _}, reason)
    end

    if Process.alive?(read_task_timeout.pid),
      do: Task.shutdown(read_task_timeout, :brutal_kill)

    node_id
  end

  @doc """
  I test a more complex scenario involving multiple writes, reads, and
  write watermark advancements.
  """
  @spec complex_write_and_read_scenario(String.t()) :: String.t()
  def complex_write_and_read_scenario(node_id \\ Node.example_random_id()) do
    spawn_node_with_initial_state(node_id)

    shard_via = Registry.via(node_id, Shard, :a)

    key = ["a"]

    # --- Acquire Write Reservations ---
    assert :ok == Shard.reserve(shard_via, key, 5, :write)
    assert :ok == Shard.reserve(shard_via, key, 6, :write)
    assert :ok == Shard.reserve(shard_via, key, 10, :write)

    # --- Acquire Read Reservations ---
    assert :ok == Shard.reserve(shard_via, key, 1, :read)
    assert :ok == Shard.reserve(shard_via, key, 4, :read)
    assert :ok == Shard.reserve(shard_via, key, 5, :read)
    assert :ok == Shard.reserve(shard_via, key, 6, :read)
    assert :ok == Shard.reserve(shard_via, key, 7, :read)
    assert :ok == Shard.reserve(shard_via, key, 9, :read)
    assert :ok == Shard.reserve(shard_via, key, 10, :read)
    assert :ok == Shard.reserve(shard_via, key, 11, :read)

    # --- Perform Writes ---
    assert :ok == Shard.write(shard_via, key, 7, 5)
    assert :ok == Shard.write(shard_via, key, 2, 6)
    assert :ok == Shard.write(shard_via, key, 8, 10)

    # --- Simulate Watermark Advancements ---
    # Reads 1, 4, 5 depend on initial state (implied WM >= 1)
    Shard.advance_write_watermark(shard_via, key, 1)

    # Read 6 needs to see write at 5
    Shard.advance_write_watermark(shard_via, key, 6)

    # Reads 7, 9, 10 need to see write at 6
    Shard.advance_write_watermark(shard_via, key, 7)

    # Read 11 needs to see write at 10
    Shard.advance_write_watermark(shard_via, key, 11)

    # --- Test Reads ---
    # Read height h resolves based on latest write < h, provided WM >= h
    # Before any writes. Initial value is 5
    assert Shard.read(shard_via, key, 1) == {:ok, 5}
    # Before write@5
    assert Shard.read(shard_via, key, 4) == {:ok, 5}
    # Before write@5
    assert Shard.read(shard_via, key, 5) == {:ok, 5}
    # Sees write@5 (value 7)
    assert Shard.read(shard_via, key, 6) == {:ok, 7}
    # Sees write@6
    assert Shard.read(shard_via, key, 7) == {:ok, 2}
    # Sees write@6
    assert Shard.read(shard_via, key, 9) == {:ok, 2}
    # Sees write@6
    assert Shard.read(shard_via, key, 10) == {:ok, 2}
    # Sees write@10
    assert Shard.read(shard_via, key, 11) == {:ok, 8}

    node_id
  end

  @doc """
  I test the internal state changes related to Garbage Collection (GC)
  and the state of entries after reservations are released.
  """
  @spec gc_and_reserve_release_state(String.t()) :: String.t()
  def gc_and_reserve_release_state(node_id \\ Node.example_random_id()) do
    spawn_node_with_initial_state(node_id)

    shard_via = Registry.via(node_id, Shard, :a)

    key = ["a"]

    # --- Writes ---
    write_ops = %{
      9 => 5,
      15 => 12,
      30 => 16,
      32 => 8
    }

    Enum.each(write_ops, fn {h, v} ->
      assert :ok == Shard.reserve(shard_via, key, h, :write)
      assert :ok == Shard.write(shard_via, key, v, h)
    end)

    # --- Direct State Check (Post-Write) ---
    state1 = :sys.get_state(shard_via)
    kv1 = Map.get(state1.kv, key, %{})

    assert Map.get(kv1, 0).value == 5
    assert !Map.get(kv1, 0).write_reserved?

    assert Map.get(kv1, 9).value == 5 and !Map.get(kv1, 9).write_reserved?
    assert Map.get(kv1, 15).value == 12 and !Map.get(kv1, 15).write_reserved?
    assert Map.get(kv1, 30).value == 16 and !Map.get(kv1, 30).write_reserved?
    assert Map.get(kv1, 32).value == 8 and !Map.get(kv1, 32).write_reserved?

    # 0, 9, 15, 30, 32
    assert map_size(kv1) == 5

    # --- Read Reservation ---
    assert :ok == Shard.reserve(shard_via, key, 17, :read)

    # Verify reservation presence in state
    state2 = :sys.get_state(shard_via)
    kv2 = Map.get(state2.kv, key, %{})
    assert Map.has_key?(kv2, 17)
    assert kv2[17].read_reserved_count > 0
    assert is_nil(kv2[17].value)
    assert !kv2[17].write_reserved?
    # Added entry for height 17
    assert map_size(kv2) == 6

    # --- Advance Read Watermark (GC Trigger) ---
    Shard.advance_read_watermark(shard_via, key, 33)

    # --- Direct State Check (Post-GC) ---
    state3 = :sys.get_state(shard_via)
    kv3 = Map.get(state3.kv, key, %{})

    # Expected remaining heights:
    # - 15: Kept because it's needed for read reservation at 17 (max_h < 17)
    # - 17: Kept because it holds the active read reservation.
    # - 32: Kept because it's the latest entry <= the watermark 33.
    assert Map.has_key?(kv3, 15)
    # Check value consistency
    assert Map.get(kv3, 15).value == 12
    assert Map.has_key?(kv3, 17)
    # Reservation still held
    assert kv3[17].read_reserved_count > 0
    assert Map.has_key?(kv3, 32)
    # Check value consistency
    assert Map.get(kv3, 32).value == 8
    assert map_size(kv3) == 3
    # Ensure others are gone
    refute Map.has_key?(kv3, 0)
    refute Map.has_key?(kv3, 9)
    refute Map.has_key?(kv3, 30)

    # --- Read Operation (at 17) ---
    # Advance WRITE watermark so read can resolve
    Shard.advance_write_watermark(shard_via, key, 18)
    # Perform the read
    assert Shard.read(shard_via, key, 17) == {:ok, 12}

    # --- Direct State Check (Post-Read) ---
    state4 = :sys.get_state(shard_via)
    kv4 = Map.get(state4.kv, key, %{})
    # Entry should still exist
    assert Map.has_key?(kv4, 17)
    # Reservation should be released
    assert kv4[17].read_reserved_count == 0
    assert is_nil(kv4[17].value)
    assert !kv4[17].write_reserved?
    # Size remains same, just reservation released
    assert map_size(kv4) == 3

    # --- Advance Read Watermark Again (Clean up entry 17) ---
    Shard.advance_read_watermark(shard_via, key, 34)

    # --- Direct State Check (Final) ---
    state5 = :sys.get_state(shard_via)
    kv5 = Map.get(state5.kv, key, %{})

    # Expected remaining heights:
    # - 32: Kept because it's the latest entry <= the new watermark 34.
    # Entries 15 and 17 should now be GC'd.
    assert Map.has_key?(kv5, 32)
    assert Map.get(kv5, 32).value == 8
    assert map_size(kv5) == 1
    # Ensure others are gone
    refute Map.has_key?(kv5, 15)
    refute Map.has_key?(kv5, 17)

    node_id
  end

  @doc """
  I test various scenarios of reservation acquisition failures due to watermarks,
  existing values, and successful re-acquisition of existing reservations.
  """
  @spec reserve_failures_and_reacquisition(String.t()) :: String.t()
  def reserve_failures_and_reacquisition(node_id \\ Node.example_random_id()) do
    spawn_node_with_initial_state(node_id)

    shard_via = Registry.via(node_id, Shard, :a)

    key = ["a"]

    # --- Setup Watermarks ---
    Shard.advance_read_watermark(shard_via, key, 10)
    Shard.advance_write_watermark(shard_via, key, 10)

    # --- Test Reserving Below Watermarks (Height 5) ---
    assert Shard.reserve(shard_via, key, 5, :read) ==
             {:error, :reserving_read_under_read_watermark}

    assert Shard.reserve(shard_via, key, 5, :write) ==
             {:error, :reserving_write_under_write_watermark}

    # Write check happens first for :read_write
    assert Shard.reserve(shard_via, key, 5, :read_write) ==
             {:error, :reserving_write_under_write_watermark}

    # --- Test Reservation Re-acquisition (Height 15) ---
    # Sequence: read -> read -> write -> write -> read

    # 1st Read
    assert :ok == Shard.reserve(shard_via, key, 15, :read)

    # 2nd Read (should be ok, idempotent)
    assert :ok == Shard.reserve(shard_via, key, 15, :read)

    # 1st Write (acquire alongside read)
    assert :ok == Shard.reserve(shard_via, key, 15, :write)

    # 2nd Write (should be ok, idempotent)
    assert :ok == Shard.reserve(shard_via, key, 15, :write)

    # 3rd Read (should be ok, idempotent)
    assert :ok == Shard.reserve(shard_via, key, 15, :read)

    # Check state: both read and write should be reserved
    state_after_reacquire = :sys.get_state(shard_via)
    kv_after_reacquire = Map.get(state_after_reacquire.kv, key, %{})
    assert Map.has_key?(kv_after_reacquire, 15)
    assert kv_after_reacquire[15].read_reserved_count > 0
    assert kv_after_reacquire[15].write_reserved?

    # --- Test Write Blocking Reservation Acquisition (Height 20) ---
    # First, reserve and write a value to height 20
    assert :ok == Shard.reserve(shard_via, key, 20, :write)

    assert :ok == Shard.write(shard_via, key, "value_at_20", 20)

    # Sequence: write -> write -> read -> read -> write

    # 1st Write (should fail due to existing value)
    assert Shard.reserve(shard_via, key, 20, :write) ==
             {:error, :slot_occupied_by_value}

    # 2nd Write (should fail)
    assert Shard.reserve(shard_via, key, 20, :write) ==
             {:error, :slot_occupied_by_value}

    # 1st Read (should succeed even with value)
    assert :ok == Shard.reserve(shard_via, key, 20, :read)

    # 2nd Read (should succeed, idempotent)
    assert :ok == Shard.reserve(shard_via, key, 20, :read)

    # 3rd Write (should fail)
    assert Shard.reserve(shard_via, key, 20, :write) ==
             {:error, :slot_occupied_by_value}

    # Check state: read should be reserved, write should not
    state_after_blocking = :sys.get_state(shard_via)
    kv_after_blocking = Map.get(state_after_blocking.kv, key, %{})
    assert Map.has_key?(kv_after_blocking, 20)
    assert kv_after_blocking[20].read_reserved_count > 0
    assert !kv_after_blocking[20].write_reserved?

    node_id
  end

  @doc """
  I test that a read can resolve successfully even if an older write reservation
  (at a height lower than the height of the value the read depends on)
  is still held. This verifies a fix for overly broad write reservation blocking.
  """
  @spec read_past_old_write_reserve(String.t()) :: String.t()
  def read_past_old_write_reserve(node_id \\ Node.example_random_id()) do
    spawn_node_with_initial_state(node_id)

    shard_via = Registry.via(node_id, Shard, :a)

    key = ["a"]

    h_reserve = 5
    h_write = 7
    write_value = 10
    h_read = 9

    # 1. Acquire write reservation at h_reserve (and HOLD it)
    assert :ok == Shard.reserve(shard_via, key, h_reserve, :write)

    # 2. Write successfully at h_write
    assert :ok == Shard.reserve(shard_via, key, h_write, :write)
    assert :ok == Shard.write(shard_via, key, write_value, h_write)

    # 3. Acquire read reservation at h_read
    assert :ok == Shard.reserve(shard_via, key, h_read, :read)

    # 4. Advance write watermark to allow the read at h_read
    # WM >= 9
    Shard.advance_write_watermark(shard_via, key, h_read + 1)

    # 5. Perform the read at h_read
    result = Shard.read(shard_via, key, h_read)

    # 6. Assert: Read at 9 should resolve to value written at 7,
    #    despite the older write reservation still held at 5.
    assert result == {:ok, write_value}

    # 7. Verify the reservation at h_reserve is still held (for sanity)
    state = :sys.get_state(shard_via)
    kv_state = Map.get(state.kv, key, %{})
    assert Map.has_key?(kv_state, h_reserve)
    assert kv_state[h_reserve].write_reserved?

    node_id
  end

  @doc """
  I test writing to an initially empty shard, advancing the write watermark,
  and then performing reads both below and above the write height.
  """
  @spec write_then_reads_empty_start(String.t()) :: String.t()
  def write_then_reads_empty_start(node_id \\ Node.example_random_id()) do
    spawn_node_with_initial_state(node_id)

    shard_via = Registry.via(node_id, Shard, :b)

    key = ["b"]
    write_height = 10
    write_value = 5
    wm_height = 20
    read_height_absent = 5
    read_height_ok = 15

    # 1. Reserve and write value at write_height
    assert :ok == Shard.reserve(shard_via, key, write_height, :write)
    assert :ok == Shard.write(shard_via, key, write_value, write_height)

    # 2. Advance write watermark past the write and reads
    Shard.advance_write_watermark(shard_via, key, wm_height)

    # 3. Read at height_absent (should be absent as latest < 5 is nothing)
    assert :ok == Shard.reserve(shard_via, key, read_height_absent, :read)
    assert Shard.read(shard_via, key, read_height_absent) == :absent

    # 4. Read at height_ok (should see write_value as latest < 15 is at 10)
    assert :ok == Shard.reserve(shard_via, key, read_height_ok, :read)
    assert Shard.read(shard_via, key, read_height_ok) == {:ok, write_value}

    node_id
  end

  @doc """
  I test the unreserve function by creating read reservations for key "a" and write reservations
  for key "b" at multiple heights, then unreserving at a specific height and verifying that only
  those reservations are released.
  """
  @spec unreserve(String.t()) :: String.t()
  def unreserve(node_id \\ Node.example_random_id()) do
    spawn_node_with_initial_state(node_id)

    shard_a_via = Registry.via(node_id, Shard, :a)
    shard_b_via = Registry.via(node_id, Shard, :b)

    # Create read reservations for key "a" at heights 1, 2, 3, 4, 5
    Enum.each(1..5, fn height ->
      assert :ok == Shard.reserve(shard_a_via, ["a"], height, :read)
    end)

    # Create write reservations for key "b" at heights 1, 2, 3, 4, 5
    Enum.each(1..5, fn height ->
      assert :ok == Shard.reserve(shard_b_via, ["b"], height, :write)
    end)

    # Verify that all reservations were made correctly
    state_before_unreserve_a = :sys.get_state(shard_a_via)
    state_before_unreserve_b = :sys.get_state(shard_b_via)

    # Check "a" reservations (read)
    a_heights = Map.get(state_before_unreserve_a.kv, ["a"], %{})

    Enum.each(1..5, fn height ->
      assert Map.has_key?(a_heights, height)
      assert a_heights[height].read_reserved_count > 0
      assert !a_heights[height].write_reserved?
    end)

    # Check "b" reservations (write)
    b_heights = Map.get(state_before_unreserve_b.kv, ["b"], %{})

    Enum.each(1..5, fn height ->
      assert Map.has_key?(b_heights, height)
      assert b_heights[height].read_reserved_count == 0
      assert b_heights[height].write_reserved?
    end)

    # Unreserve at height 3
    assert :ok == Shard.unreserve(shard_a_via, ["a"], 3, :read)
    assert :ok == Shard.unreserve(shard_a_via, ["a"], 3, :write)
    assert :ok == Shard.unreserve(shard_b_via, ["b"], 3, :read)
    assert :ok == Shard.unreserve(shard_b_via, ["b"], 3, :write)

    # Verify that only height 3 reservations were removed
    state_after_unreserve_a = :sys.get_state(shard_a_via)
    state_after_unreserve_b = :sys.get_state(shard_b_via)

    # Check "a" reservations after unreserve
    a_heights_after = Map.get(state_after_unreserve_a.kv, ["a"], %{})
    assert Map.has_key?(a_heights_after, 3)
    assert a_heights_after[3].read_reserved_count == 0

    # Other heights should still have read_reserved_count > 0
    Enum.each([1, 2, 4, 5], fn height ->
      assert Map.has_key?(a_heights_after, height)
      assert a_heights_after[height].read_reserved_count > 0
    end)

    # Check "b" reservations after unreserve
    b_heights_after = Map.get(state_after_unreserve_b.kv, ["b"], %{})
    # Height 3 should have write_reserved? = false
    assert Map.has_key?(b_heights_after, 3)
    assert !b_heights_after[3].write_reserved?

    # Other heights should still have write_reserved? = true
    Enum.each([1, 2, 4, 5], fn height ->
      assert Map.has_key?(b_heights_after, height)
      assert b_heights_after[height].write_reserved?
    end)

    node_id
  end

  @doc """
  I test that GC preserves the latest *committed* state below the read
  watermark, even if a later read reservation was acquired and released.
  """
  @spec gc_preserves_committed_state_before_watermark(String.t()) ::
          String.t()
  def gc_preserves_committed_state_before_watermark(
        node_id \\ Node.example_random_id()
      ) do
    spawn_node_with_initial_state(node_id)

    shard_via = Registry.via(node_id, Shard, :c)

    key = ["c"]

    # 1. Write a value at height 3
    assert :ok == Shard.reserve(shard_via, key, 3, :write)
    assert :ok == Shard.write(shard_via, key, 100, 3)

    # 2. Reserve read at height 4
    assert :ok == Shard.reserve(shard_via, key, 4, :read)

    # 3. Release reservation at height 4 (e.g., tx rollback)
    assert :ok == Shard.unreserve(shard_via, key, 4, :read)

    # 4. Advance read watermark past height 3 and 4, triggering GC
    Shard.advance_read_watermark(shard_via, key, 5)

    # 5. Verify state
    state = :sys.get_state(shard_via)
    key_height_map = Map.get(state.kv, key, %{})

    # Check that the entry at height 3 (committed write) still exists
    assert Map.has_key?(key_height_map, 3)

    assert %{value: 100, read_reserved_count: 0, write_reserved?: false} ==
             key_height_map[3],
           "Committed state at height 3 should be preserved by GC"

    # Check that the initial value at 0 is gone
    refute Map.has_key?(key_height_map, 0),
           "Initial state at height 0 should be GC'd"

    # Check that the entry for height 4 (only reserved, then released) is gone
    refute Map.has_key?(key_height_map, 4),
           "State at height 4 should be GC'd"

    node_id
  end

  @doc """
  I test that unreserving a write reservation triggers the check for pending reads,
  allowing a previously blocked read (blocked by the reservation, not the watermark)
  to complete.
  """
  @spec unreserve_triggers_pending_read(String.t()) :: String.t()
  def unreserve_triggers_pending_read(node_id \\ Node.example_random_id()) do
    spawn_node_with_initial_state(node_id)

    shard_via = Registry.via(node_id, Shard, :a)

    # Initial state %{"a" => 5} handled by spawn_node_with_initial_state
    key = ["a"]

    h_write = 2
    write_value = 100
    h_blocking_reserve = 3
    h_read = 4
    wm_height = 5

    # 1. Reserve and Write at h_write
    assert :ok == Shard.reserve(shard_via, key, h_write, :write)
    assert :ok == Shard.write(shard_via, key, write_value, h_write)

    # 2. Reserve Write at h_blocking_reserve (will block the read)
    assert :ok == Shard.reserve(shard_via, key, h_blocking_reserve, :write)

    # 3. Reserve Read at h_read
    assert :ok == Shard.reserve(shard_via, key, h_read, :read)

    # 4. Start Read Task (will block due to h_blocking_reserve)
    read_task =
      Task.async(fn ->
        Shard.read(shard_via, key, h_read)
      end)

    # 4.5 Verify the read task is initially blocked (before watermark advance)
    assert Task.yield(read_task, 100) == nil,
           "Read task should be blocked initially by reservation"

    # 5. Advance Write Watermark (enough for h_read, but still blocked by reservation)
    Shard.advance_write_watermark(shard_via, key, wm_height)

    # 6. Verify Read is Still Blocked (yield returns nil if task hasn't finished)
    assert Task.yield(read_task, 100) == nil,
           "Read task should still be blocked by the write reservation at height #{h_blocking_reserve}"

    # 7. Unreserve the Blocking Height
    assert :ok == Shard.unreserve(shard_via, key, h_blocking_reserve, :write)

    # 8. Await Read Result (should now complete)
    result = Task.await(read_task, 1000)

    # 9. Assert the result is the value from h_write
    # Read at h_read(4) sees latest committed write < 4, which is at h_write(2)
    assert result == {:ok, write_value},
           "Read should have resolved to #{write_value} after unreserve"

    node_id
  end

  @doc """
  I test the backup_state functionality by initializing a shard with specific
  values, performing writes, backing up the state, and verifying the backup
  contents in Mnesia.
  """
  @spec backup_state(String.t()) :: String.t()
  def backup_state(node_id \\ Node.example_random_id()) do
    spawn_node_with_initial_state(node_id)

    shard_a_via = Registry.via(node_id, Shard, :a)
    shard_b_via = Registry.via(node_id, Shard, :b)

    # --- Perform Writes ---
    # Key "a" writes
    assert :ok == Shard.reserve(shard_a_via, ["a"], 2, :write)
    assert :ok == Shard.write(shard_a_via, ["a"], 1, 2)
    assert :ok == Shard.reserve(shard_a_via, ["a"], 5, :write)
    assert :ok == Shard.write(shard_a_via, ["a"], 2, 5)
    assert :ok == Shard.reserve(shard_a_via, ["a"], 7, :write)
    assert :ok == Shard.write(shard_a_via, ["a"], 3, 7)

    # Key "b" writes (starts empty)
    assert :ok == Shard.reserve(shard_b_via, ["b"], 5, :write)
    assert :ok == Shard.write(shard_b_via, ["b"], 6, 5)
    assert :ok == Shard.reserve(shard_b_via, ["b"], 6, :write)
    assert :ok == Shard.write(shard_b_via, ["b"], 10, 6)

    assert :ok == Shard.backup_state(shard_a_via)
    assert :ok == Shard.backup_state(shard_b_via)

    # Define the expected values in the backup table
    expected_backed_up_values = %{
      # Shard 'a' from node_id
      {:a, ["a"], 0} => 5,
      {:a, ["a"], 2} => 1,
      {:a, ["a"], 5} => 2,
      {:a, ["a"], 7} => 3,
      # Shard 'b' from node_id
      {:b, ["b"], 5} => 6,
      {:b, ["b"], 6} => 10
    }

    # Get the backup table name
    backup_table = Anoma.Node.Tables.table_shard_backups(node_id)

    # Verify the Mnesia table contents within a transaction
    {:atomic, read_results} =
      :mnesia.transaction(fn ->
        # Read all expected records
        read_results =
          Enum.map(expected_backed_up_values, fn {{shard_id, key, height},
                                                  _value} ->
            record_key = {shard_id, key, height}

            {{shard_id, key, height},
             :mnesia.read({backup_table, record_key})}
          end)
          |> Map.new()

        read_results
      end)

    # Assert the expected records exist and have the correct details
    Enum.each(expected_backed_up_values, fn {{shard_id, key, height},
                                             expected_value} ->
      lookup_key = {shard_id, key, height}

      assert Map.has_key?(read_results, lookup_key),
             "Result for shard #{shard_id} key '#{key}' height #{height} missing from backup"

      read_result = read_results[lookup_key]

      expected_record = [
        {backup_table, {shard_id, key, height}, expected_value}
      ]

      assert read_result == expected_record,
             "Mismatch for backup of shard #{shard_id} key '#{key}' height #{height}. Expected #{inspect(expected_record)}, got #{inspect(read_result)}"
    end)

    node_id
  end
end
