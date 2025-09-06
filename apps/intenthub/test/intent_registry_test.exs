defmodule IntentHub.IntentRegistryTest do
  use ExUnit.Case

  alias IntentHub.{Intent, IntentRegistry}

  setup do
    # Start the registry for each test
    {:ok, registry} = start_supervised(IntentRegistry)
    %{registry: registry}
  end

  describe "register/1" do
    test "registers a new intent", %{registry: _registry} do
      intent = %Intent{
        id: "test-id",
        user_address: "0x1234567890abcdef",
        intent_type: :nft,
        parameters: %{contract_address: "0xabc", token_id: 1, max_price: 100},
        status: :pending,
        created_at: DateTime.utc_now()
      }

      assert {:ok, registered_intent} = IntentRegistry.register(intent)
      assert registered_intent.id == intent.id
    end
  end

  describe "get/1" do
    test "retrieves an existing intent", %{registry: _registry} do
      intent = %Intent{
        id: "test-id",
        user_address: "0x1234567890abcdef",
        intent_type: :nft,
        parameters: %{contract_address: "0xabc", token_id: 1, max_price: 100},
        status: :pending,
        created_at: DateTime.utc_now()
      }

      IntentRegistry.register(intent)
      assert {:ok, retrieved_intent} = IntentRegistry.get("test-id")
      assert retrieved_intent.id == intent.id
    end

    test "returns error for non-existent intent", %{registry: _registry} do
      assert {:error, "Intent not found"} = IntentRegistry.get("non-existent-id")
    end
  end

  describe "update/1" do
    test "updates an existing intent", %{registry: _registry} do
      intent = %Intent{
        id: "test-id",
        user_address: "0x1234567890abcdef",
        intent_type: :nft,
        parameters: %{contract_address: "0xabc", token_id: 1, max_price: 100},
        status: :pending,
        created_at: DateTime.utc_now()
      }

      IntentRegistry.register(intent)
      
      updated_intent = %{intent | status: :executed, executed_at: DateTime.utc_now()}
      assert {:ok, result_intent} = IntentRegistry.update(updated_intent)
      assert result_intent.status == :executed
    end
  end

  describe "list_by_user/1" do
    test "returns empty list for user with no intents", %{registry: _registry} do
      assert [] = IntentRegistry.list_by_user("0x1234567890abcdef")
    end

    test "returns user's intents", %{registry: _registry} do
      user_address = "0x1234567890abcdef"
      
      intent1 = %Intent{
        id: "test-id-1",
        user_address: user_address,
        intent_type: :nft,
        parameters: %{contract_address: "0xabc", token_id: 1, max_price: 100},
        status: :pending,
        created_at: DateTime.utc_now()
      }

      intent2 = %Intent{
        id: "test-id-2",
        user_address: user_address,
        intent_type: :staking,
        parameters: %{token_address: "0xdef", amount: 1000, duration: 30},
        status: :pending,
        created_at: DateTime.utc_now()
      }

      IntentRegistry.register(intent1)
      IntentRegistry.register(intent2)

      intents = IntentRegistry.list_by_user(user_address)
      assert length(intents) == 2
      assert Enum.all?(intents, &(&1.user_address == user_address))
    end
  end

  describe "list_by_type/1" do
    test "returns empty list for type with no intents", %{registry: _registry} do
      assert [] = IntentRegistry.list_by_type(:nft)
    end

    test "returns intents of specific type", %{registry: _registry} do
      intent1 = %Intent{
        id: "test-id-1",
        user_address: "0x1234567890abcdef",
        intent_type: :nft,
        parameters: %{contract_address: "0xabc", token_id: 1, max_price: 100},
        status: :pending,
        created_at: DateTime.utc_now()
      }

      intent2 = %Intent{
        id: "test-id-2",
        user_address: "0xfedcba0987654321",
        intent_type: :nft,
        parameters: %{contract_address: "0xdef", token_id: 2, max_price: 200},
        status: :pending,
        created_at: DateTime.utc_now()
      }

      IntentRegistry.register(intent1)
      IntentRegistry.register(intent2)

      nft_intents = IntentRegistry.list_by_type(:nft)
      assert length(nft_intents) == 2
      assert Enum.all?(nft_intents, &(&1.intent_type == :nft))
    end
  end

  describe "get_stats/0" do
    test "returns initial stats", %{registry: _registry} do
      stats = IntentRegistry.get_stats()
      
      assert stats.total_intents == 0
      assert stats.pending_intents == 0
      assert stats.executed_intents == 0
      assert stats.failed_intents == 0
    end

    test "returns updated stats after registering intents", %{registry: _registry} do
      intent1 = %Intent{
        id: "test-id-1",
        user_address: "0x1234567890abcdef",
        intent_type: :nft,
        parameters: %{contract_address: "0xabc", token_id: 1, max_price: 100},
        status: :pending,
        created_at: DateTime.utc_now()
      }

      intent2 = %Intent{
        id: "test-id-2",
        user_address: "0xfedcba0987654321",
        intent_type: :staking,
        parameters: %{token_address: "0xdef", amount: 1000, duration: 30},
        status: :pending,
        created_at: DateTime.utc_now()
      }

      IntentRegistry.register(intent1)
      IntentRegistry.register(intent2)

      stats = IntentRegistry.get_stats()
      assert stats.total_intents == 2
      assert stats.pending_intents == 2
      assert stats.executed_intents == 0
      assert stats.failed_intents == 0
    end

    test "updates stats when intent status changes", %{registry: _registry} do
      intent = %Intent{
        id: "test-id",
        user_address: "0x1234567890abcdef",
        intent_type: :nft,
        parameters: %{contract_address: "0xabc", token_id: 1, max_price: 100},
        status: :pending,
        created_at: DateTime.utc_now()
      }

      IntentRegistry.register(intent)
      
      # Update to executed
      executed_intent = %{intent | status: :executed, executed_at: DateTime.utc_now()}
      IntentRegistry.update(executed_intent)

      stats = IntentRegistry.get_stats()
      assert stats.total_intents == 1
      assert stats.pending_intents == 0
      assert stats.executed_intents == 1
      assert stats.failed_intents == 0
    end
  end
end
