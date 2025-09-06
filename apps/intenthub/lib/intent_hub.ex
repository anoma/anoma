defmodule IntentHub do
  @moduledoc """
  IntentHub - Intent registry and management system for Anoma.
  
  This module provides the main interface for managing user intents
  in the Web3 ecosystem, allowing users to register, track, and execute
  their blockchain-related intentions.
  """

  alias IntentHub.{Intent, IntentRegistry}

  @doc """
  Registers a new intent for a user.
  
  ## Examples
  
      iex> IntentHub.register_intent("0x123...", :nft, %{contract_address: "0xabc...", token_id: 1, max_price: 100})
      {:ok, %IntentHub.Intent{}}
      
      iex> IntentHub.register_intent("0x123...", :invalid_type, %{})
      {:error, "Unknown intent type"}
  """
  def register_intent(user_address, intent_type, parameters, opts \\ []) do
    with {:ok, validated_params} <- Intent.validate_parameters(intent_type, parameters),
         intent <- Intent.new(user_address, intent_type, validated_params, opts) do
      IntentRegistry.register(intent)
    end
  end

  @doc """
  Lists all intents for a given user address.
  """
  def list_user_intents(user_address) do
    IntentRegistry.list_by_user(user_address)
  end

  @doc """
  Gets an intent by its ID.
  """
  def get_intent(intent_id) do
    IntentRegistry.get(intent_id)
  end

  @doc """
  Updates the status of an intent.
  """
  def update_intent_status(intent_id, new_status) do
    with {:ok, intent} <- IntentRegistry.get(intent_id),
         updated_intent <- Intent.update_status(intent, new_status) do
      IntentRegistry.update(updated_intent)
    end
  end

  @doc """
  Executes a pending intent.
  """
  def execute_intent(intent_id) do
    with {:ok, intent} <- IntentRegistry.get(intent_id),
         true <- Intent.executable?(intent) do
      # For now, just mark as executed
      # In a real implementation, this would trigger the actual blockchain transaction
      update_intent_status(intent_id, :executed)
    else
      false -> {:error, "Intent is not executable"}
      error -> error
    end
  end

  @doc """
  Lists all intents by type.
  """
  def list_intents_by_type(intent_type) do
    IntentRegistry.list_by_type(intent_type)
  end

  @doc """
  Gets statistics about registered intents.
  """
  def get_intent_stats do
    IntentRegistry.get_stats()
  end
end
