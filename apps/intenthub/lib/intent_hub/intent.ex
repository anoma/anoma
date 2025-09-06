defmodule IntentHub.Intent do
  @moduledoc """
  Intent struct representing a user's intent in the Web3 ecosystem.
  
  An intent represents what a user wants to achieve, such as:
  - NFT purchase
  - Token staking
  - Token swap
  - DAO participation
  """

  @type intent_type :: :nft | :staking | :swap | :dao
  @type status :: :pending | :executed | :failed | :cancelled

  defstruct [
    :id,
    :user_address,
    :intent_type,
    :parameters,
    :status,
    :created_at,
    :executed_at,
    :priority
  ]

  @doc """
  Creates a new intent with the given parameters.
  """
  def new(user_address, intent_type, parameters, opts \\ []) do
    %__MODULE__{
      id: generate_id(),
      user_address: user_address,
      intent_type: intent_type,
      parameters: parameters,
      status: :pending,
      created_at: DateTime.utc_now(),
      priority: Keyword.get(opts, :priority, :normal)
    }
  end

  @doc """
  Updates the status of an intent.
  """
  def update_status(intent, new_status) do
    updated_intent = %{intent | status: new_status}
    
    if new_status == :executed do
      %{updated_intent | executed_at: DateTime.utc_now()}
    else
      updated_intent
    end
  end

  @doc """
  Checks if an intent is executable (pending status).
  """
  def executable?(%__MODULE__{status: :pending}), do: true
  def executable?(_), do: false

  @doc """
  Validates intent parameters based on intent type.
  """
  def validate_parameters(intent_type, parameters) do
    case intent_type do
      :nft -> validate_nft_parameters(parameters)
      :staking -> validate_staking_parameters(parameters)
      :swap -> validate_swap_parameters(parameters)
      :dao -> validate_dao_parameters(parameters)
      _ -> {:error, "Unknown intent type"}
    end
  end

  # Private functions

  defp generate_id do
    :crypto.strong_rand_bytes(16) |> Base.encode16(case: :lower)
  end

  defp validate_nft_parameters(params) do
    required_fields = [:contract_address, :token_id, :max_price]
    
    if Enum.all?(required_fields, &Map.has_key?(params, &1)) do
      {:ok, params}
    else
      {:error, "Missing required NFT parameters: #{inspect(required_fields)}"}
    end
  end

  defp validate_staking_parameters(params) do
    required_fields = [:token_address, :amount, :duration]
    
    if Enum.all?(required_fields, &Map.has_key?(params, &1)) do
      {:ok, params}
    else
      {:error, "Missing required staking parameters: #{inspect(required_fields)}"}
    end
  end

  defp validate_swap_parameters(params) do
    required_fields = [:token_in, :token_out, :amount_in, :min_amount_out]
    
    if Enum.all?(required_fields, &Map.has_key?(params, &1)) do
      {:ok, params}
    else
      {:error, "Missing required swap parameters: #{inspect(required_fields)}"}
    end
  end

  defp validate_dao_parameters(params) do
    required_fields = [:dao_address, :proposal_id, :vote_choice]
    
    if Enum.all?(required_fields, &Map.has_key?(params, &1)) do
      {:ok, params}
    else
      {:error, "Missing required DAO parameters: #{inspect(required_fields)}"}
    end
  end
end
