defmodule Anoma.Node.Transaction.Shard.Supervisor do
  @moduledoc """
  I am the dynamic supervisor for `Anoma.Node.Transaction.Shard` processes.

  I start with no children. Children (shards) are started dynamically either
  during initial node setup based on a provided schema or later via
  `start_shard/3`.

  I record key-to-shard mappings in Mnesia
  (`Anoma.Tables.table_shard_key_map/1`) for future replay support.
  These mappings are not currently read in production.

  ### Public API

  - `start_link/1`: I start the supervisor.
  - `start_shard/3`: I dynamically start a new shard for a given key.
  """

  use DynamicSupervisor

  alias Anoma.Node.Registry
  alias Anoma.Node.Transaction.Shard
  alias Anoma.Tables

  require Logger

  ############################################################
  #                       Types                              #
  ############################################################

  @type key :: [binary()]
  @type initial_value :: any()
  @type schema :: [key() | {key(), initial_value()}]

  @type startup_options ::
          {:node_id, String.t()}
          | {:strategy, :one_per_key}
          | {:schema, schema()}

  ############################################################
  #                 Supervisor Implementation                #
  ############################################################

  @spec start_link(list(startup_options())) :: Supervisor.on_start()
  def start_link(args) do
    node_id = Keyword.fetch!(args, :node_id)
    name = Registry.via(node_id, __MODULE__)

    child_specs = schema_to_child_specs(node_id, args)

    with {:ok, pid} <-
           DynamicSupervisor.start_link(__MODULE__, [node_id: node_id],
             name: name
           ),
         :ok <- start_initial_children(pid, child_specs) do
      {:ok, pid}
    end
  end

  @impl true
  def init(args) do
    node_id = Keyword.fetch!(args, :node_id)
    Process.set_label({__MODULE__, node_id})
    DynamicSupervisor.init(strategy: :one_for_one)
  end

  @doc """
  I dynamically start a new shard process for the given key.
  """
  @spec start_shard(String.t(), key(), initial_value() | nil) ::
          DynamicSupervisor.on_start_child()
  def start_shard(node_id, key, initial_value \\ nil) do
    shard_id = shard_id(key)

    # Check if already registered before attempting start
    case Registry.whereis(node_id, Shard, shard_id) do
      pid when is_pid(pid) ->
        {:error, {:already_started, pid}}

      nil ->
        initial_kv = if initial_value, do: %{key => initial_value}, else: %{}
        spec = child_spec(node_id, shard_id, initial_kv)
        supervisor = Registry.via(node_id, __MODULE__)

        with {:ok, pid} <- DynamicSupervisor.start_child(supervisor, spec) do
          record_shard_mapping(node_id, key, shard_id)
          {:ok, pid}
        end
    end
  end

  ############################################################
  #                    Private Helpers                       #
  ############################################################

  @spec schema_to_child_specs(String.t(), list(startup_options())) ::
          [Supervisor.child_spec()]
  defp schema_to_child_specs(node_id, args) do
    schema = Keyword.get(args, :schema)
    strategy = Keyword.get(args, :strategy)

    case {strategy, schema} do
      {:one_per_key, entries} when is_list(entries) ->
        Enum.flat_map(entries, fn
          {key, value} when is_list(key) ->
            [entry_to_child_spec(node_id, key, %{key => value})]

          key when is_list(key) ->
            [entry_to_child_spec(node_id, key, %{})]

          _ ->
            []
        end)

      _ ->
        []
    end
  end

  @spec start_initial_children(pid(), [Supervisor.child_spec()]) ::
          :ok | {:error, term()}
  defp start_initial_children(supervisor_pid, child_specs) do
    Enum.reduce_while(child_specs, :ok, fn spec, :ok ->
      case DynamicSupervisor.start_child(supervisor_pid, spec) do
        {:ok, _pid} ->
          {:cont, :ok}

        {:error, reason} ->
          DynamicSupervisor.stop(supervisor_pid)

          {:halt,
           {:error, {:failed_to_start_initial_child, spec[:id], reason}}}
      end
    end)
  end

  @spec entry_to_child_spec(String.t(), key(), map()) ::
          Supervisor.child_spec()
  defp entry_to_child_spec(node_id, key, initial_kv) do
    shard_id = shard_id(key)
    record_shard_mapping(node_id, key, shard_id)
    child_spec(node_id, shard_id, initial_kv)
  end

  @spec shard_id(key()) :: atom()
  defp shard_id(key) do
    key |> Enum.join() |> String.to_atom()
  end

  @spec child_spec(String.t(), atom(), map()) :: Supervisor.child_spec()
  defp child_spec(node_id, shard_id, initial_kv) do
    %{
      id: shard_id,
      start:
        {Shard, :start_link,
         [[node_id: node_id, id: shard_id, initial_kv: initial_kv]]}
    }
  end

  # Best-effort Mnesia write for future replay support.
  @spec record_shard_mapping(String.t(), key(), atom()) ::
          {:atomic, :ok} | {:aborted, term()}
  defp record_shard_mapping(node_id, key, shard_id) do
    table = Tables.table_shard_key_map(node_id)

    :mnesia.transaction(fn ->
      :mnesia.write({table, key, shard_id})
    end)
  end
end
