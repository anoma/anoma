defmodule IntentHub.Intent do
  @moduledoc """
  I represent a user's intent in the Web3 ecosystem.

  I provide functionality for creating, validating, and managing intents
  that represent user intentions such as NFT purchases, token staking,
  token swaps, and DAO participation.
  """

  use TypedStruct

  @type intent_type :: :nft | :staking | :swap | :dao
  @type status :: :pending | :executed | :failed | :cancelled
  @type priority :: :low | :normal | :high

  typedstruct do
    @typedoc """
    I represent a user's intent with all necessary metadata.

    ### Fields
    - `:id` - Unique identifier for this intent
    - `:user_address` - Address of the user who created this intent
    - `:intent_type` - Type of intent (nft, staking, swap, dao)
    - `:parameters` - Intent-specific parameters
    - `:status` - Current status of the intent
    - `:created_at` - Timestamp when intent was created
    - `:executed_at` - Timestamp when intent was executed (if applicable)
    - `:priority` - Priority level of the intent
    """
    field(:id, String.t())
    field(:user_address, String.t())
    field(:intent_type, intent_type())
    field(:parameters, map())
    field(:status, status())
    field(:created_at, DateTime.t())
    field(:executed_at, DateTime.t() | nil)
    field(:priority, priority())
  end

  @doc """
  I create a new intent with the given parameters.

  ## Parameters
  - `user_address` - Address of the user creating the intent
  - `intent_type` - Type of intent to create
  - `parameters` - Intent-specific parameters
  - `opts` - Optional parameters including priority

  ## Returns
  A new `%IntentHub.Intent{}` struct with generated ID and timestamps.
  """
  @spec new(String.t(), intent_type(), map(), Keyword.t()) :: t()
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
  I update the status of an intent and set execution timestamp if executed.

  ## Parameters
  - `intent` - The intent to update
  - `new_status` - The new status to set

  ## Returns
  Updated intent with new status and execution timestamp if applicable.
  """
  @spec update_status(t(), status()) :: t()
  def update_status(intent, new_status) do
    updated_intent = %{intent | status: new_status}

    if new_status == :executed do
      %{updated_intent | executed_at: DateTime.utc_now()}
    else
      updated_intent
    end
  end

  @doc """
  I check if an intent is executable (has pending status).

  ## Parameters
  - `intent` - The intent to check

  ## Returns
  `true` if intent is pending and can be executed, `false` otherwise.
  """
  @spec executable?(t()) :: boolean()
  def executable?(%__MODULE__{status: :pending}), do: true
  def executable?(_), do: false

  @doc """
  I validate intent parameters based on the intent type.

  ## Parameters
  - `intent_type` - The type of intent to validate
  - `parameters` - The parameters to validate

  ## Returns
  `{:ok, parameters}` if valid, `{:error, reason}` if invalid.
  """
  @spec validate_parameters(intent_type(), map()) :: {:ok, map()} | {:error, String.t()}
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

  @spec generate_id() :: String.t()
  defp generate_id do
    :crypto.strong_rand_bytes(16) |> Base.encode16(case: :lower)
  end

  @spec validate_nft_parameters(map()) :: {:ok, map()} | {:error, String.t()}
  defp validate_nft_parameters(params) do
    required_fields = [:contract_address, :token_id, :max_price]

    if Enum.all?(required_fields, &Map.has_key?(params, &1)) do
      {:ok, params}
    else
      {:error, "Missing required NFT parameters: #{inspect(required_fields)}"}
    end
  end

  @spec validate_staking_parameters(map()) :: {:ok, map()} | {:error, String.t()}
  defp validate_staking_parameters(params) do
    required_fields = [:token_address, :amount, :duration]

    if Enum.all?(required_fields, &Map.has_key?(params, &1)) do
      {:ok, params}
    else
      {:error, "Missing required staking parameters: #{inspect(required_fields)}"}
    end
  end

  @spec validate_swap_parameters(map()) :: {:ok, map()} | {:error, String.t()}
  defp validate_swap_parameters(params) do
    required_fields = [:token_in, :token_out, :amount_in, :min_amount_out]

    if Enum.all?(required_fields, &Map.has_key?(params, &1)) do
      {:ok, params}
    else
      {:error, "Missing required swap parameters: #{inspect(required_fields)}"}
    end
  end

  @spec validate_dao_parameters(map()) :: {:ok, map()} | {:error, String.t()}
  defp validate_dao_parameters(params) do
    required_fields = [:dao_address, :proposal_id, :vote_choice]

    if Enum.all?(required_fields, &Map.has_key?(params, &1)) do
      {:ok, params}
    else
      {:error, "Missing required DAO parameters: #{inspect(required_fields)}"}
    end
  end
end
