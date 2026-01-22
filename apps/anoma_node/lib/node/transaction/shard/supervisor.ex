defmodule Anoma.Node.Transaction.Shard.Supervisor do
  @moduledoc """
  I am the dynamic supervisor for `Anoma.Node.Transaction.Shard` processes.

  I start with no children. Children (shards) are started dynamically either
  during initial node setup based on a provided schema or later via the
  `start_shard/3` function. I maintain a Mnesia table
  (`Anoma.Tables.table_shard_key_map/1`) mapping keys to the registered name
  (`{:via, Registry, {Anoma.Node, {Shard, key}}}`) of the `Shard` process
  responsible for that key. The actual lookup of keys is handled by the
  `Anoma.Node.Transaction.Ordering`.

  ### Key Concepts

  - **Supervisor Args:** Keyword list including `:node_id`, and optionally `:strategy` and `:schema` for *initial* setup.
  - **Strategy:** Determines how initial shards are created (e.g., `:one_per_key`). Only used during initial setup.
  - **Schema:** Defines the initial keys and their starting values. Only used during initial setup.
  - **Mnesia Table:** `table_shard_key_map/1` for key -> shard name lookup (used by Anoma.Node.Transaction.Ordering).
  - **Dynamic Starting:** Use `start_shard/3` to add new shards after initial setup.

  ### Public API

  - `start_link/1`: I start the supervisor.
  - `start_shard/3`: I dynamically start a new shard for a given key and optionally value.
  - `get_shard_key_map/1`: I return the name of the Mnesia table for the shard key map for the given node ID.
  """

  use DynamicSupervisor
  use EventBroker.DefFilter
  use TypedStruct

  alias Anoma.Node.Registry
  alias Anoma.Node.Transaction.Shard
  alias Anoma.Tables

  require Logger

  ############################################################
  #                       Types                              #
  ############################################################

  @typedoc "I represent a key managed by a shard."
  @type key :: [binary()]

  @typedoc "I represent the initial value associated with a key in a shard."
  @type initial_value :: any()

  @typedoc """
  I am the schema defining the keys and their initial values for shards.
  For the `:one_per_key` strategy, I expect a list containing either `key` binaries
  or `{key, initial_value}` tuples. If only a key is provided, there is no
  initial value.
  """
  @type schema :: [key() | {key(), initial_value()}]

  @typedoc """
  I am the sharding strategy.
  Currently, I only support `:one_per_key`.
  """
  @type strategy :: :one_per_key

  @typedoc """
  I am the type of the arguments that the ShardSupervisor expects at startup.
  I require `:node_id` and optionally `:strategy` and `:schema` keys for initial setup.
  """
  @type args_t :: [
          node_id: String.t(),
          strategy: strategy() | nil,
          schema: schema() | nil
        ]

  @typedoc """
  I am the type of the arguments that the Shard process expects.
  I am not explicitly used, but this may be useful to know.
  """
  @type shard_args :: [
          id: key(),
          initial_kv: %{key() => initial_value()}
        ]

  ############################################################
  #                 Supervisor Implementation                #
  ############################################################

  @doc """
  I am the start_link function for the ShardSupervisor.

  I start and link the dynamic supervisor process under the current supervision tree,
  registering myself locally using a node-specific name.

  If initial :strategy and :schema are provided, I perform Mnesia setup
  synchronously *before* starting the supervisor. If Mnesia fails, I return
  an error. After the supervisor starts, I start the initial children.
  """
  @spec start_link(args_t()) :: Supervisor.on_start()
  def start_link(args) do
    node_id = Keyword.fetch!(args, :node_id)
    strategy = Keyword.get(args, :strategy)
    schema = Keyword.get(args, :schema)
    name = Registry.via(node_id, __MODULE__)

    # Perform initial setup synchronously *before* starting the supervisor process
    {initial_child_specs, mnesia_setup_result} =
      if strategy != nil and is_list(schema) do
        {specs, key_to_id_map} = process_schema(node_id, strategy, schema)
        mnesia_result = populate_mnesia_table(key_to_id_map, node_id)
        {specs, mnesia_result}
      else
        # No schema, Mnesia setup is considered successful
        {[], :ok}
      end

    case mnesia_setup_result do
      :ok ->
        case DynamicSupervisor.start_link(__MODULE__, [node_id: node_id],
               name: name
             ) do
          {:ok, pid} ->
            Enum.reduce(initial_child_specs, {:ok, pid}, fn child_spec, acc ->
              case acc do
                {:ok, supervisor_pid} ->
                  case DynamicSupervisor.start_child(
                         supervisor_pid,
                         child_spec
                       ) do
                    {:ok, _child_pid} ->
                      {:ok, supervisor_pid}

                    {:error, reason} ->
                      error_info =
                        {:failed_to_start_initial_child, child_spec[:id],
                         reason}

                      Logger.error(
                        "#{__MODULE__} failed to start initial child #{inspect(child_spec[:id])} for node #{node_id}: #{inspect(reason)}. Supervisor startup will fail."
                      )

                      DynamicSupervisor.stop(supervisor_pid)

                      {:error, error_info}
                  end

                error_acc ->
                  error_acc
              end
            end)

          {:error, _reason} = error ->
            error
        end

      :no_shards ->
        DynamicSupervisor.start_link(__MODULE__, [node_id: node_id],
          name: name
        )

      {:error, reason} ->
        {:error, {:mnesia_table_population_failed, reason}}
    end
  end

  @impl true
  @doc """
  I am the DynamicSupervisor initialization callback.

  I just set the process label and initialize the supervisor state.
  Initial child starting is handled in `start_link/1` after this process starts.
  """
  @spec init(args :: [node_id: String.t()]) ::
          {:ok, DynamicSupervisor.sup_flags()}
  def init(args) do
    node_id = Keyword.fetch!(args, :node_id)
    Process.set_label({__MODULE__, node_id})

    DynamicSupervisor.init(strategy: :one_for_one)
  end

  @doc """
  I dynamically start a new shard process for the given key.

  I register the shard with the registry and add its key mapping to the Mnesia table.
  """
  @spec start_shard(
          node_id :: String.t(),
          key :: key(),
          initial_value :: initial_value() | nil
        ) ::
          DynamicSupervisor.on_start_child()
          | {:error, :mnesia_update_failed, any()}
  def start_shard(node_id, key, initial_value \\ nil) do
    supervisor_name = Registry.via(node_id, __MODULE__)
    shard_id = Enum.reduce(key, "", &(&2 <> &1)) |> String.to_atom()

    # Check if already registered before attempting start
    case Registry.whereis(node_id, Shard, shard_id) do
      existing_pid when is_pid(existing_pid) ->
        {:error, {:already_started, existing_pid}}

      nil ->
        initial_kv =
          if initial_value do
            %{key => initial_value}
          else
            %{}
          end

        shard_args = [
          node_id: node_id,
          id: shard_id,
          initial_kv: initial_kv
        ]

        child_spec = %{
          id: shard_id,
          start: {Shard, :start_link, [shard_args]}
        }

        result = DynamicSupervisor.start_child(supervisor_name, child_spec)

        case result do
          {:ok, new_pid} ->
            case add_key_to_mnesia_map(node_id, key, shard_id) do
              :ok ->
                {:ok, new_pid}

              {:error, reason} ->
                # Attempt to terminate the child we just started if Mnesia fails
                # to prevent inconsistent state (shard running but not in map)
                DynamicSupervisor.terminate_child(supervisor_name, new_pid)
                {:error, :mnesia_update_failed, reason}
            end

          other_result ->
            other_result
        end
    end
  end

  ############################################################
  #                    Private Helpers                       #
  ############################################################

  # Helper to create child spec and update accumulator map for a given key during initial setup.
  # Returns the child spec itself and the {key, shard_id} tuple.
  defp create_shard_spec_and_mapping(
         node_id,
         key,
         initial_kv
       ) do
    shard_id = Enum.reduce(key, "", &(&2 <> &1)) |> String.to_atom()

    shard_args = [
      node_id: node_id,
      id: shard_id,
      initial_kv: initial_kv
    ]

    child_spec = %{
      id: shard_id,
      start: {Shard, :start_link, [shard_args]}
    }

    {child_spec, {key, shard_id}}
  end

  # Processes the schema based on the strategy to generate shard child specs
  # and a map of key -> shard_id. (Used only during initial async setup)
  @spec process_schema(
          node_id :: String.t(),
          strategy :: strategy() | nil,
          schema :: schema() | nil
        ) ::
          {
            [Supervisor.child_spec()],
            %{key() => atom()}
          }
  defp process_schema(node_id, :one_per_key, schema) when is_list(schema) do
    Enum.reduce(schema, {[], %{}}, fn schema_entry, {specs_acc, map_acc} ->
      # Determine key and initial_kv based on entry format
      {key, initial_kv} =
        case schema_entry do
          {k, v} when is_list(k) -> {k, %{k => v}}
          k when is_list(k) -> {k, %{}}
          # Mark invalid entry
          _ -> {nil, nil}
        end

      # If key is valid (not nil), create spec and get mapping
      if key do
        {child_spec, {map_key, map_value}} =
          create_shard_spec_and_mapping(
            node_id,
            key,
            initial_kv
          )

        {[child_spec | specs_acc], Map.put(map_acc, map_key, map_value)}
      else
        # Skip invalid entry
        {specs_acc, map_acc}
      end
    end)
  end

  # Cases where strategy/schema are nil or invalid for initial setup
  defp process_schema(_node_id, _strategy, _schema) do
    {[], %{}}
  end

  @doc """
  I return the Mnesia table name for the shard key map for the given node ID.
  """
  @spec get_shard_key_map(node_id :: String.t()) :: atom()
  def get_shard_key_map(node_id) do
    Tables.table_shard_key_map(node_id)
  end

  # Populates the Mnesia table with the key -> shard_id mapping.
  # Called during initial synchronous setup within init/1.
  # Returns :ok on success, :no_shards if the map is empty, or {:error, reason}.
  @spec populate_mnesia_table(
          map :: %{key() => atom()},
          node_id :: String.t()
        ) ::
          :ok | :no_shards | {:error, any()}
  defp populate_mnesia_table(key_to_id_map, node_id)
       when map_size(key_to_id_map) > 0 do
    table_name = Tables.table_shard_key_map(node_id)

    # Clear existing entries first to handle potential restarts/schema changes
    case :mnesia.clear_table(table_name) do
      {:atomic, :ok} ->
        mnesia_tx = fn ->
          Enum.each(key_to_id_map, fn {key, shard_id} ->
            :mnesia.write({table_name, key, shard_id})
          end)
        end

        case :mnesia.transaction(mnesia_tx) do
          {:atomic, :ok} ->
            :ok

          {:aborted, reason} ->
            {:error, {:mnesia_transaction_failed, reason}}
        end

      {:aborted, reason} ->
        {:error, {:clear_table_failed, reason}}
    end
  end

  defp populate_mnesia_table(_empty_map, _node_id) do
    :no_shards
  end

  # Adds a single key -> shard_id mapping to the Mnesia table transactionally.
  # Called by start_shard/3. Does NOT clear the table.
  @spec add_key_to_mnesia_map(
          node_id :: String.t(),
          key :: key(),
          shard_id :: atom()
        ) ::
          :ok | {:error, any()}
  defp add_key_to_mnesia_map(node_id, key, shard_id) do
    table_name = Tables.table_shard_key_map(node_id)

    mnesia_tx = fn ->
      :mnesia.write({table_name, key, shard_id})
    end

    case :mnesia.transaction(mnesia_tx) do
      {:atomic, :ok} ->
        :ok

      {:aborted, reason} ->
        Logger.error(
          "Failed to add key #{inspect(key)} -> #{inspect(shard_id)} to Mnesia table #{inspect(table_name)}: #{inspect(reason)}"
        )

        {:error, {:mnesia_transaction_failed, reason}}
    end
  end
end
