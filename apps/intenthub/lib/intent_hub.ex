defmodule IntentHub do
  @moduledoc """
  I provide the main interface for managing user intents in the Web3 ecosystem.

  I allow users to register, track, and execute their blockchain-related
  intentions through a simple and consistent API.
  """

  alias IntentHub.{Intent, IntentRegistry}

  @type intent_id :: String.t()
  @type user_address :: String.t()
  @type intent_type :: Intent.intent_type()
  @type intent_status :: Intent.status()
  @type intent_priority :: Intent.priority()
  @type intent_list :: [Intent.t()]
  @type intent_stats :: IntentRegistry.stats()

  @doc """
  I register a new intent for a user.

  ## Parameters
  - `user_address` - Address of the user creating the intent
  - `intent_type` - Type of intent to create
  - `parameters` - Intent-specific parameters
  - `opts` - Optional parameters including priority

  ## Returns
  `{:ok, intent}` on success, `{:error, reason}` on failure.

  ## Examples

      iex> IntentHub.register_intent("0x123...", :nft, %{contract_address: "0xabc...", token_id: 1, max_price: 100})
      {:ok, %IntentHub.Intent{}}

      iex> IntentHub.register_intent("0x123...", :invalid_type, %{})
      {:error, "Unknown intent type"}
  """
  @spec register_intent(user_address(), intent_type(), map(), Keyword.t()) ::
          {:ok, Intent.t()} | {:error, String.t()}
  def register_intent(user_address, intent_type, parameters, opts \\ []) do
    with {:ok, validated_params} <- Intent.validate_parameters(intent_type, parameters),
         intent <- Intent.new(user_address, intent_type, validated_params, opts) do
      IntentRegistry.register(intent)
    end
  end

  @doc """
  I list all intents for a given user address.

  ## Parameters
  - `user_address` - Address of the user

  ## Returns
  List of intents for the user.
  """
  @spec list_user_intents(user_address()) :: intent_list()
  def list_user_intents(user_address) do
    IntentRegistry.list_by_user(user_address)
  end

  @doc """
  I get an intent by its ID.

  ## Parameters
  - `intent_id` - ID of the intent to retrieve

  ## Returns
  `{:ok, intent}` if found, `{:error, reason}` if not found.
  """
  @spec get_intent(intent_id()) :: {:ok, Intent.t()} | {:error, String.t()}
  def get_intent(intent_id) do
    IntentRegistry.get(intent_id)
  end

  @doc """
  I update the status of an intent.

  ## Parameters
  - `intent_id` - ID of the intent to update
  - `new_status` - New status to set

  ## Returns
  `{:ok, intent}` on success, `{:error, reason}` on failure.
  """
  @spec update_intent_status(intent_id(), intent_status()) ::
          {:ok, Intent.t()} | {:error, String.t()}
  def update_intent_status(intent_id, new_status) do
    with {:ok, intent} <- IntentRegistry.get(intent_id),
         updated_intent <- Intent.update_status(intent, new_status) do
      IntentRegistry.update(updated_intent)
    end
  end

  @doc """
  I execute a pending intent.

  ## Parameters
  - `intent_id` - ID of the intent to execute

  ## Returns
  `{:ok, intent}` on success, `{:error, reason}` on failure.

  ## Note
  Currently marks the intent as executed. In a real implementation,
  this would trigger the actual blockchain transaction.
  """
  @spec execute_intent(intent_id()) :: {:ok, Intent.t()} | {:error, String.t()}
  def execute_intent(intent_id) do
    with {:ok, intent} <- IntentRegistry.get(intent_id),
         true <- Intent.executable?(intent) do
      update_intent_status(intent_id, :executed)
    else
      false -> {:error, "Intent is not executable"}
      error -> error
    end
  end

  @doc """
  I list all intents by type.

  ## Parameters
  - `intent_type` - Type of intents to list

  ## Returns
  List of intents of the specified type.
  """
  @spec list_intents_by_type(intent_type()) :: intent_list()
  def list_intents_by_type(intent_type) do
    IntentRegistry.list_by_type(intent_type)
  end

  @doc """
  I get statistics about registered intents.

  ## Returns
  Map containing intent statistics.
  """
  @spec get_intent_stats() :: intent_stats()
  def get_intent_stats do
    IntentRegistry.get_stats()
  end
end
