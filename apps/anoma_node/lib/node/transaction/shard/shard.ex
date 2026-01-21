defmodule Anoma.Node.Transaction.Shard do
  @moduledoc """
  I am the Shard module.

  I manage a partition of the distributed key-value store, handling requests
  for reserving slots, reading, and writing specific keys at specific heights.

  ### Public API

  I provide the following public functionality:

  - `start_link/1`
  - `reserve/3`
  - `read/3`
  - `write/4`
  - `unreserve/4`
  - `retract/4`
  - `backup_state/1`
  - `advance_watermark/3`
  - `debug_get_state/1`

  ### Key Concepts

  - **Cell:** A cell of a shard, contains all the information required for a shard.
  - **Synchronous Reads:** Read requests (`read/3`) block the caller
      until resolved. Resolution may be delayed internally if blocked
      by watermarks or preceding write reservations. Read completion
      releases the specific read reservation.
  """

  alias Anoma.Node.Transaction.Shard.Detail
  alias Anoma.Node.Transaction.Shard.Cell
  alias Anoma.Node.Registry

  require Logger

  use GenServer
  use TypedStruct

  ############################################################
  #                       Types                              #
  ############################################################

  @typedoc "The key in the key-value store."
  @type key :: [binary()]

  @type startup_options ::
          {:node_id, String.t()}
          | {:inital_kv, %{key() => %{Cell.height() => any()}}}
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
    - `:cells` - The cell of a shard, containing and KV information.
    """
    field(:id, atom())
    field(:node_id, String.t())
    field(:cells, %{Cell.height() => Cell.t()})
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

    cells =
      Map.new(args[:initial_kv], fn {key, value} ->
        {key, %Cell{details: %{0 => %Detail{cell: %{value: value}}}}}
      end)

    {:ok, %__MODULE__{id: args[:id], node_id: args[:node_id], cells: cells}}
  end

  ############################################################
  #                    Public RPC API                      #
  ############################################################

  @doc """
  I am the reserve function for the Shard module. Use me to reserve a
  read or write at a specific key at a specific height.

  Reservations exist to inform the KV store that a value will
  be written at a specific height at some point in the future.
  If I know that an empty entry will be written to, then an immediate read
  will have to wait until the write occurs.

  I request a reservation on a specific key at a given height.
  Capabilities can be `:read`, `:write`, or `:read_write`.
  I return `:ok` on success, or an error tuple.
  """
  # Might be a call? Need to know if we can reserve it
  @spec reserve(GenServer.server(), key(), Cell.height()) ::
          :ok
          | {:error,
             :reserving_write_under_write_watermark
             | :occupied}
  def reserve(shard_pid, key, height) do
    GenServer.call(shard_pid, {:reserve, key, height})
  end

  @doc """
  I am the read function for the Shard module.

  I perform a synchronous read request for a key at a specific height.
  The caller blocks until the read can be resolved (potentially waiting for watermarks)
  and receives the result directly.
  Returns `{:ok, value}`, `:absent`, or an error tuple.
  """
  @spec read(GenServer.server(), key(), Cell.height()) ::
          {:ok, any()} | :absent | {:error, :not_reserved}
  def read(shard_pid, key, height) do
    GenServer.call(shard_pid, {:read, key, height}, :infinity)
  end

  @doc """
  I am the write function for the Shard module.

  I write a value for a key at a specific height, requiring a prior `reserve` call
  with `:write` capability for this `{key, height}`.
  I return `:ok` on success, or an error tuple.
  """
  @spec write(GenServer.server(), key(), any(), Cell.height()) :: :ok
  def write(shard_pid, key, value, height) do
    GenServer.cast(shard_pid, {:write, key, value, height})
  end

  @doc """
  I release a write reservation for a given key at a given height.

  This is an asynchronous operation primarily used for rollbacks of failed transactions.
  """
  @spec unreserve(GenServer.server(), key(), Cell.height()) :: :ok
  def unreserve(shard_pid, key, height) do
    GenServer.cast(shard_pid, {:unreserve, key, height})
  end

  @doc """
  I retract a potential read on a shard.

  I am useful when a specific `pid` may have requested a read that is
  no longer relevant.
  """
  @spec retract(GenServer.server(), key(), Cell.height(), pid()) :: :ok
  def retract(shard_pid, key, height, pid) do
    GenServer.cast(shard_pid, {:retract, key, height, pid})
  end

  @doc """
  I trigger a backup of the shard's internal state to Mnesia.

  This is a synchronous operation.
  """
  @spec backup_state(GenServer.server()) :: :ok | {:error, any()}
  def backup_state(shard_pid) do
    # unsure if should be a call as we may care about blocking due to
    # 2 phase commit
    GenServer.call(shard_pid, :backup_state)
  end

  @doc """
  I advance the write watermark for a given key.

  This is an asynchronous operation.
  """
  @spec advance_watermark(GenServer.server(), key(), Cell.height()) :: :ok
  def advance_watermark(shard_pid, key, h_write) do
    GenServer.cast(shard_pid, {:write_watermark_advanced, key, h_write})
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  @impl true
  def handle_call({:reserve, key, height}, _from, state) do
    {response, state} = handle_reserve(key, height, state)
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
    {:noreply, update_cell(state, key, &Cell.write(&1, height, value))}
  end

  def handle_cast({:retract, key, height, pid}, state) do
    {:noreply, update_cell(state, key, &Cell.retract(&1, height, pid))}
  end

  def handle_cast({:write_watermark_advanced, key, write}, state) do
    new_state =
      update_cell(state, key, &Cell.run_advance_watermark(&1, write))

    {:noreply, new_state}
  end

  def handle_cast({:unreserve, key, height}, state) do
    {:noreply, update_cell(state, key, &Cell.unreserve(&1, height))}
  end

  ############################################################
  #                 Genserver Implementation                 #
  ############################################################

  @spec update_cell(t(), key(), (Cell.t() -> Cell.t())) :: t()
  defp update_cell(state, key, fun) do
    cell = Map.get(state.cells, key, %Cell{})
    %__MODULE__{state | cells: Map.put(state.cells, key, fun.(cell))}
  end

  @spec handle_reserve(key(), Cell.height(), t()) ::
          {:ok | {:error, atom()}, t()}
  defp handle_reserve(key, height, state = %__MODULE__{cells: cells}) do
    case Map.get(cells, key, %Cell{}) |> Cell.reserve(height) do
      {:ok, new_cells} ->
        {:ok, %__MODULE__{state | cells: Map.put(cells, key, new_cells)}}

      {:error, error} ->
        {{:error, error}, state}
    end
  end

  @spec handle_read(key(), Cell.height(), GenServer.from(), t()) ::
          {:reply, {:ok, any()} | :absent | {:error, atom()}, t()}
          | {:noreply, t()}
  defp handle_read(key, height, from, state = %__MODULE__{cells: c}) do
    cell = Map.get(c, key, %Cell{})

    case Cell.read(cell, height) do
      :blocked ->
        new_cell = Map.put(c, key, Cell.add_pending(cell, height, from))
        {:noreply, %__MODULE__{state | cells: new_cell}}

      :absent ->
        {:reply, :absent, state}

      {:ok, resolved} ->
        {:reply, {:ok, resolved}, state}
    end
  end

  @spec handle_backup_state(t()) :: :ok
  defp handle_backup_state(%__MODULE__{id: id, node_id: node_id, cells: cs}) do
    backup_table = Anoma.Tables.table_shard_backups(node_id)

    mnesia_tx = fn ->
      # Overwrite for same height.
      Enum.each(cs, fn {key, cell} ->
        Enum.each(cell.details, fn
          {height, %Detail{cell: %{value: v}}} ->
            record_key = {id, key, height}
            :mnesia.write({backup_table, record_key, v})

          {_, _} ->
            nil
        end)
      end)
    end

    with {:aborted, reason} <- :mnesia.transaction(mnesia_tx) do
      Logger.error("Shard #{inspect(id)} backup failed: #{inspect(reason)}")
    end
  end
end
