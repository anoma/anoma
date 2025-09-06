defmodule IntentHub.IntentRegistry do
  @moduledoc """
  I manage intent storage and retrieval using in-memory storage.

  I provide a GenServer-based registry that stores intents in memory
  and provides efficient querying capabilities for the IntentHub system.
  """

  use GenServer
  require Logger

  alias IntentHub.Intent

  @type intent_id :: String.t()
  @type user_address :: String.t()
  @type intent_type :: Intent.intent_type()
  @type intent_list :: [Intent.t()]
  @type stats :: %{
    total_intents: non_neg_integer(),
    pending_intents: non_neg_integer(),
    executed_intents: non_neg_integer(),
    failed_intents: non_neg_integer()
  }

  # Client API

  @doc """
  I start the IntentRegistry GenServer.

  ## Parameters
  - `opts` - GenServer start options

  ## Returns
  `{:ok, pid}` on success, `{:error, reason}` on failure.
  """
  @spec start_link(Keyword.t()) :: GenServer.on_start()
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, %{}, opts)
  end

  @doc """
  I register a new intent in the registry.

  ## Parameters
  - `intent` - The intent to register

  ## Returns
  `{:ok, intent}` on success, `{:error, reason}` on failure.
  """
  @spec register(Intent.t()) :: {:ok, Intent.t()} | {:error, String.t()}
  def register(intent) do
    GenServer.call(__MODULE__, {:register, intent})
  end

  @doc """
  I get an intent by its ID.

  ## Parameters
  - `intent_id` - The ID of the intent to retrieve

  ## Returns
  `{:ok, intent}` if found, `{:error, reason}` if not found.
  """
  @spec get(intent_id()) :: {:ok, Intent.t()} | {:error, String.t()}
  def get(intent_id) do
    GenServer.call(__MODULE__, {:get, intent_id})
  end

  @doc """
  I update an existing intent in the registry.

  ## Parameters
  - `intent` - The updated intent

  ## Returns
  `{:ok, intent}` on success, `{:error, reason}` on failure.
  """
  @spec update(Intent.t()) :: {:ok, Intent.t()} | {:error, String.t()}
  def update(intent) do
    GenServer.call(__MODULE__, {:update, intent})
  end

  @doc """
  I list all intents for a specific user.

  ## Parameters
  - `user_address` - The address of the user

  ## Returns
  List of intents for the user.
  """
  @spec list_by_user(user_address()) :: intent_list()
  def list_by_user(user_address) do
    GenServer.call(__MODULE__, {:list_by_user, user_address})
  end

  @doc """
  I list all intents of a specific type.

  ## Parameters
  - `intent_type` - The type of intents to list

  ## Returns
  List of intents of the specified type.
  """
  @spec list_by_type(intent_type()) :: intent_list()
  def list_by_type(intent_type) do
    GenServer.call(__MODULE__, {:list_by_type, intent_type})
  end

  @doc """
  I get statistics about registered intents.

  ## Returns
  Map containing intent statistics.
  """
  @spec get_stats() :: stats()
  def get_stats do
    GenServer.call(__MODULE__, :get_stats)
  end

  # Server callbacks

  @impl true
  def init(_opts) do
    state = %{
      intents: %{},                    # intent_id -> intent
      user_intents: %{},               # user_address -> [intent_id]
      type_intents: %{                 # intent_type -> [intent_id]
        nft: [],
        staking: [],
        swap: [],
        dao: []
      },
      stats: %{
        total_intents: 0,
        pending_intents: 0,
        executed_intents: 0,
        failed_intents: 0
      }
    }
    
    Logger.info("IntentHub.IntentRegistry started")
    {:ok, state}
  end

  @impl true
  def handle_call({:register, intent}, _from, state) do
    new_state = add_intent(state, intent)
    Logger.info("Registered intent #{intent.id} for user #{intent.user_address}")
    {:reply, {:ok, intent}, new_state}
  end

  @impl true
  def handle_call({:get, intent_id}, _from, state) do
    case Map.get(state.intents, intent_id) do
      nil -> {:reply, {:error, "Intent not found"}, state}
      intent -> {:reply, {:ok, intent}, state}
    end
  end

  @impl true
  def handle_call({:update, intent}, _from, state) do
    new_state = update_intent(state, intent)
    Logger.info("Updated intent #{intent.id} with status #{intent.status}")
    {:reply, {:ok, intent}, new_state}
  end

  @impl true
  def handle_call({:list_by_user, user_address}, _from, state) do
    intent_ids = Map.get(state.user_intents, user_address, [])
    intents = Enum.map(intent_ids, &Map.get(state.intents, &1))
    {:reply, intents, state}
  end

  @impl true
  def handle_call({:list_by_type, intent_type}, _from, state) do
    intent_ids = Map.get(state.type_intents, intent_type, [])
    intents = Enum.map(intent_ids, &Map.get(state.intents, &1))
    {:reply, intents, state}
  end

  @impl true
  def handle_call(:get_stats, _from, state) do
    {:reply, state.stats, state}
  end

  # Private functions

  defp add_intent(state, intent) do
    # Add to main intents map
    new_intents = Map.put(state.intents, intent.id, intent)
    
    # Add to user intents
    user_intent_ids = Map.get(state.user_intents, intent.user_address, [])
    new_user_intents = Map.put(state.user_intents, intent.user_address, [intent.id | user_intent_ids])
    
    # Add to type intents
    type_intent_ids = Map.get(state.type_intents, intent.intent_type, [])
    new_type_intents = Map.put(state.type_intents, intent.intent_type, [intent.id | type_intent_ids])
    
    # Update stats
    new_stats = update_stats(state.stats, intent, :add)
    
    %{state | 
      intents: new_intents,
      user_intents: new_user_intents,
      type_intents: new_type_intents,
      stats: new_stats
    }
  end

  defp update_intent(state, updated_intent) do
    # Update main intents map
    new_intents = Map.put(state.intents, updated_intent.id, updated_intent)
    
    # Update stats (remove old status, add new status)
    old_intent = Map.get(state.intents, updated_intent.id)
    new_stats = state.stats
    |> update_stats(old_intent, :remove)
    |> update_stats(updated_intent, :add)
    
    %{state | 
      intents: new_intents,
      stats: new_stats
    }
  end

  defp update_stats(stats, intent, operation) do
    case {intent.status, operation} do
      {:pending, :add} -> %{stats | total_intents: stats.total_intents + 1, pending_intents: stats.pending_intents + 1}
      {:pending, :remove} -> %{stats | total_intents: stats.total_intents - 1, pending_intents: stats.pending_intents - 1}
      {:executed, :add} -> %{stats | total_intents: stats.total_intents + 1, executed_intents: stats.executed_intents + 1}
      {:executed, :remove} -> %{stats | total_intents: stats.total_intents - 1, executed_intents: stats.executed_intents - 1}
      {:failed, :add} -> %{stats | total_intents: stats.total_intents + 1, failed_intents: stats.failed_intents + 1}
      {:failed, :remove} -> %{stats | total_intents: stats.total_intents - 1, failed_intents: stats.failed_intents - 1}
      _ -> stats
    end
  end
end
