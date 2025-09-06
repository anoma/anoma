defmodule IntentHubTest do
  use ExUnit.Case
  doctest IntentHub

  alias IntentHub.Intent

  describe "register_intent/3" do
    test "registers a valid NFT intent" do
      user_address = "0x1234567890abcdef"
      parameters = %{
        contract_address: "0xabcdef1234567890",
        token_id: 1,
        max_price: 100
      }

      assert {:ok, intent} = IntentHub.register_intent(user_address, :nft, parameters)
      assert intent.user_address == user_address
      assert intent.intent_type == :nft
      assert intent.parameters == parameters
      assert intent.status == :pending
    end

    test "registers a valid staking intent" do
      user_address = "0x1234567890abcdef"
      parameters = %{
        token_address: "0xabcdef1234567890",
        amount: 1000,
        duration: 30
      }

      assert {:ok, intent} = IntentHub.register_intent(user_address, :staking, parameters)
      assert intent.intent_type == :staking
      assert intent.parameters == parameters
    end

    test "registers a valid swap intent" do
      user_address = "0x1234567890abcdef"
      parameters = %{
        token_in: "0xabcdef1234567890",
        token_out: "0xfedcba0987654321",
        amount_in: 100,
        min_amount_out: 95
      }

      assert {:ok, intent} = IntentHub.register_intent(user_address, :swap, parameters)
      assert intent.intent_type == :swap
    end

    test "registers a valid DAO intent" do
      user_address = "0x1234567890abcdef"
      parameters = %{
        dao_address: "0xabcdef1234567890",
        proposal_id: 42,
        vote_choice: :yes
      }

      assert {:ok, intent} = IntentHub.register_intent(user_address, :dao, parameters)
      assert intent.intent_type == :dao
    end

    test "returns error for invalid intent type" do
      user_address = "0x1234567890abcdef"
      parameters = %{test: "data"}

      assert {:error, "Unknown intent type"} = 
        IntentHub.register_intent(user_address, :invalid_type, parameters)
    end

    test "returns error for missing NFT parameters" do
      user_address = "0x1234567890abcdef"
      parameters = %{contract_address: "0xabc"}

      assert {:error, _} = IntentHub.register_intent(user_address, :nft, parameters)
    end
  end

  describe "list_user_intents/1" do
    test "returns empty list for user with no intents" do
      user_address = "0x1234567890abcdef"
      assert [] = IntentHub.list_user_intents(user_address)
    end

    test "returns user's intents" do
      user_address = "0x1234567890abcdef"
      parameters = %{contract_address: "0xabc", token_id: 1, max_price: 100}

      {:ok, _intent1} = IntentHub.register_intent(user_address, :nft, parameters)
      {:ok, _intent2} = IntentHub.register_intent(user_address, :nft, parameters)

      intents = IntentHub.list_user_intents(user_address)
      assert length(intents) == 2
      assert Enum.all?(intents, &(&1.user_address == user_address))
    end
  end

  describe "get_intent/1" do
    test "returns intent by id" do
      user_address = "0x1234567890abcdef"
      parameters = %{contract_address: "0xabc", token_id: 1, max_price: 100}

      {:ok, intent} = IntentHub.register_intent(user_address, :nft, parameters)
      
      assert {:ok, retrieved_intent} = IntentHub.get_intent(intent.id)
      assert retrieved_intent.id == intent.id
    end

    test "returns error for non-existent intent" do
      assert {:error, "Intent not found"} = IntentHub.get_intent("non-existent-id")
    end
  end

  describe "execute_intent/1" do
    test "executes a pending intent" do
      user_address = "0x1234567890abcdef"
      parameters = %{contract_address: "0xabc", token_id: 1, max_price: 100}

      {:ok, intent} = IntentHub.register_intent(user_address, :nft, parameters)
      
      assert {:ok, executed_intent} = IntentHub.execute_intent(intent.id)
      assert executed_intent.status == :executed
      assert executed_intent.executed_at != nil
    end

    test "returns error for non-existent intent" do
      assert {:error, "Intent not found"} = IntentHub.execute_intent("non-existent-id")
    end
  end

  describe "list_intents_by_type/1" do
    test "returns intents of specific type" do
      user_address = "0x1234567890abcdef"
      nft_params = %{contract_address: "0xabc", token_id: 1, max_price: 100}
      staking_params = %{token_address: "0xdef", amount: 1000, duration: 30}

      {:ok, _nft_intent} = IntentHub.register_intent(user_address, :nft, nft_params)
      {:ok, _staking_intent} = IntentHub.register_intent(user_address, :staking, staking_params)

      nft_intents = IntentHub.list_intents_by_type(:nft)
      assert length(nft_intents) == 1
      assert Enum.all?(nft_intents, &(&1.intent_type == :nft))
    end
  end

  describe "get_intent_stats/0" do
    test "returns intent statistics" do
      user_address = "0x1234567890abcdef"
      parameters = %{contract_address: "0xabc", token_id: 1, max_price: 100}

      {:ok, intent} = IntentHub.register_intent(user_address, :nft, parameters)
      
      stats = IntentHub.get_intent_stats()
      assert stats.total_intents == 1
      assert stats.pending_intents == 1
      assert stats.executed_intents == 0
      assert stats.failed_intents == 0
    end
  end
end
