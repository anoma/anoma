defmodule IntentHubTest do
  use ExUnit.Case, async: true

  alias IntentHub
  alias IntentHub.Intent

  describe "register_intent/3" do
    test "registers a valid NFT intent" do
      user_address = "0x1234567890abcdef"
      intent_type = :nft
      parameters = %{
        contract_address: "0xabcdef1234567890",
        token_id: 1,
        max_price: 100
      }

      assert {:ok, %Intent{} = intent} = IntentHub.register_intent(user_address, intent_type, parameters)

      assert intent.user_address == user_address
      assert intent.intent_type == intent_type
      assert intent.parameters == parameters
      assert intent.status == :pending
      assert intent.priority == :normal
      assert is_binary(intent.id)
      assert %DateTime{} = intent.created_at
    end

    test "registers a valid staking intent" do
      user_address = "0x1234567890abcdef"
      intent_type = :staking
      parameters = %{
        token_address: "0xabcdef1234567890",
        amount: 1000,
        duration: 30
      }

      assert {:ok, %Intent{} = intent} = IntentHub.register_intent(user_address, intent_type, parameters)

      assert intent.user_address == user_address
      assert intent.intent_type == intent_type
      assert intent.parameters == parameters
      assert intent.status == :pending
    end

    test "registers a valid swap intent" do
      user_address = "0x1234567890abcdef"
      intent_type = :swap
      parameters = %{
        token_in: "0xabcdef1234567890",
        token_out: "0xfedcba0987654321",
        amount_in: 100,
        min_amount_out: 95
      }

      assert {:ok, %Intent{} = intent} = IntentHub.register_intent(user_address, intent_type, parameters)

      assert intent.user_address == user_address
      assert intent.intent_type == intent_type
      assert intent.parameters == parameters
      assert intent.status == :pending
    end

    test "registers a valid DAO intent" do
      user_address = "0x1234567890abcdef"
      intent_type = :dao
      parameters = %{
        dao_address: "0xabcdef1234567890",
        proposal_id: 42,
        vote_choice: :yes
      }

      assert {:ok, %Intent{} = intent} = IntentHub.register_intent(user_address, intent_type, parameters)

      assert intent.user_address == user_address
      assert intent.intent_type == intent_type
      assert intent.parameters == parameters
      assert intent.status == :pending
    end

    test "returns error for invalid intent type" do
      user_address = "0x1234567890abcdef"
      intent_type = :invalid_type
      parameters = %{}

      assert {:error, "Unknown intent type"} = IntentHub.register_intent(user_address, intent_type, parameters)
    end

    test "returns error for missing required parameters" do
      user_address = "0x1234567890abcdef"
      intent_type = :nft
      parameters = %{contract_address: "0xabc"}

      assert {:error, _reason} = IntentHub.register_intent(user_address, intent_type, parameters)
    end

    test "registers intent with custom priority" do
      user_address = "0x1234567890abcdef"
      intent_type = :nft
      parameters = %{
        contract_address: "0xabcdef1234567890",
        token_id: 1,
        max_price: 100
      }
      opts = [priority: :high]

      assert {:ok, %Intent{} = intent} = IntentHub.register_intent(user_address, intent_type, parameters, opts)

      assert intent.priority == :high
    end
  end

  describe "list_user_intents/1" do
    test "returns empty list for user with no intents" do
      user_address = "0x1234567890abcdef"

      assert [] = IntentHub.list_user_intents(user_address)
    end

    test "returns intents for user" do
      user_address = "0x1234567890abcdef"
      intent_type = :nft
      parameters = %{
        contract_address: "0xabcdef1234567890",
        token_id: 1,
        max_price: 100
      }

      {:ok, _intent} = IntentHub.register_intent(user_address, intent_type, parameters)

      intents = IntentHub.list_user_intents(user_address)
      assert length(intents) == 1

      [intent] = intents
      assert intent.user_address == user_address
      assert intent.intent_type == intent_type
    end
  end

  describe "get_intent/1" do
    test "returns intent by ID" do
      user_address = "0x1234567890abcdef"
      intent_type = :nft
      parameters = %{
        contract_address: "0xabcdef1234567890",
        token_id: 1,
        max_price: 100
      }

      {:ok, created_intent} = IntentHub.register_intent(user_address, intent_type, parameters)

      assert {:ok, retrieved_intent} = IntentHub.get_intent(created_intent.id)
      assert retrieved_intent.id == created_intent.id
      assert retrieved_intent.user_address == user_address
    end

    test "returns error for non-existent intent" do
      non_existent_id = "nonexistent"

      assert {:error, "Intent not found"} = IntentHub.get_intent(non_existent_id)
    end
  end

  describe "update_intent_status/2" do
    test "updates intent status" do
      user_address = "0x1234567890abcdef"
      intent_type = :nft
      parameters = %{
        contract_address: "0xabcdef1234567890",
        token_id: 1,
        max_price: 100
      }

      {:ok, intent} = IntentHub.register_intent(user_address, intent_type, parameters)

      assert {:ok, updated_intent} = IntentHub.update_intent_status(intent.id, :executed)
      assert updated_intent.status == :executed
      assert %DateTime{} = updated_intent.executed_at
    end

    test "returns error for non-existent intent" do
      non_existent_id = "nonexistent"

      assert {:error, "Intent not found"} = IntentHub.update_intent_status(non_existent_id, :executed)
    end
  end

  describe "execute_intent/1" do
    test "executes a pending intent" do
      user_address = "0x1234567890abcdef"
      intent_type = :nft
      parameters = %{
        contract_address: "0xabcdef1234567890",
        token_id: 1,
        max_price: 100
      }

      {:ok, intent} = IntentHub.register_intent(user_address, intent_type, parameters)

      assert {:ok, executed_intent} = IntentHub.execute_intent(intent.id)
      assert executed_intent.status == :executed
      assert %DateTime{} = executed_intent.executed_at
    end

    test "returns error for non-executable intent" do
      user_address = "0x1234567890abcdef"
      intent_type = :nft
      parameters = %{
        contract_address: "0xabcdef1234567890",
        token_id: 1,
        max_price: 100
      }

      {:ok, intent} = IntentHub.register_intent(user_address, intent_type, parameters)
      {:ok, _updated_intent} = IntentHub.update_intent_status(intent.id, :executed)

      assert {:error, "Intent is not executable"} = IntentHub.execute_intent(intent.id)
    end
  end

  describe "list_intents_by_type/1" do
    test "returns intents by type" do
      user_address = "0x1234567890abcdef"

      # Create NFT intent
      nft_parameters = %{
        contract_address: "0xabcdef1234567890",
        token_id: 1,
        max_price: 100
      }
      {:ok, _nft_intent} = IntentHub.register_intent(user_address, :nft, nft_parameters)

      # Create staking intent
      staking_parameters = %{
        token_address: "0xabcdef1234567890",
        amount: 1000,
        duration: 30
      }
      {:ok, _staking_intent} = IntentHub.register_intent(user_address, :staking, staking_parameters)

      nft_intents = IntentHub.list_intents_by_type(:nft)
      staking_intents = IntentHub.list_intents_by_type(:staking)

      assert length(nft_intents) == 1
      assert length(staking_intents) == 1

      [nft_intent] = nft_intents
      [staking_intent] = staking_intents

      assert nft_intent.intent_type == :nft
      assert staking_intent.intent_type == :staking
    end
  end

  describe "get_intent_stats/0" do
    test "returns correct statistics" do
      user_address = "0x1234567890abcdef"

      # Initially empty
      stats = IntentHub.get_intent_stats()
      assert stats.total_intents == 0
      assert stats.pending_intents == 0
      assert stats.executed_intents == 0
      assert stats.failed_intents == 0

      # Create and execute an intent
      parameters = %{
        contract_address: "0xabcdef1234567890",
        token_id: 1,
        max_price: 100
      }

      {:ok, intent} = IntentHub.register_intent(user_address, :nft, parameters)
      {:ok, _executed_intent} = IntentHub.execute_intent(intent.id)

      stats = IntentHub.get_intent_stats()
      assert stats.total_intents == 1
      assert stats.pending_intents == 0
      assert stats.executed_intents == 1
      assert stats.failed_intents == 0
    end
  end
end