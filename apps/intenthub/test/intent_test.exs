defmodule IntentHub.IntentTest do
  use ExUnit.Case
  doctest IntentHub.Intent

  alias IntentHub.Intent

  describe "new/4" do
    test "creates a new intent with default values" do
      user_address = "0x1234567890abcdef"
      intent_type = :nft
      parameters = %{test: "data"}

      intent = Intent.new(user_address, intent_type, parameters)

      assert intent.user_address == user_address
      assert intent.intent_type == intent_type
      assert intent.parameters == parameters
      assert intent.status == :pending
      assert intent.created_at != nil
      assert intent.priority == :normal
      assert String.length(intent.id) == 32
    end

    test "creates intent with custom priority" do
      user_address = "0x1234567890abcdef"
      intent_type = :nft
      parameters = %{test: "data"}
      opts = [priority: :high]

      intent = Intent.new(user_address, intent_type, parameters, opts)

      assert intent.priority == :high
    end
  end

  describe "update_status/2" do
    test "updates intent status to executed" do
      intent = %Intent{
        id: "test-id",
        user_address: "0x123",
        intent_type: :nft,
        parameters: %{},
        status: :pending,
        created_at: DateTime.utc_now()
      }

      updated_intent = Intent.update_status(intent, :executed)

      assert updated_intent.status == :executed
      assert updated_intent.executed_at != nil
    end

    test "updates intent status to failed without executed_at" do
      intent = %Intent{
        id: "test-id",
        user_address: "0x123",
        intent_type: :nft,
        parameters: %{},
        status: :pending,
        created_at: DateTime.utc_now()
      }

      updated_intent = Intent.update_status(intent, :failed)

      assert updated_intent.status == :failed
      assert updated_intent.executed_at == nil
    end
  end

  describe "executable?/1" do
    test "returns true for pending intent" do
      intent = %Intent{status: :pending}
      assert Intent.executable?(intent)
    end

    test "returns false for executed intent" do
      intent = %Intent{status: :executed}
      refute Intent.executable?(intent)
    end

    test "returns false for failed intent" do
      intent = %Intent{status: :failed}
      refute Intent.executable?(intent)
    end

    test "returns false for cancelled intent" do
      intent = %Intent{status: :cancelled}
      refute Intent.executable?(intent)
    end
  end

  describe "validate_parameters/2" do
    test "validates NFT parameters successfully" do
      parameters = %{
        contract_address: "0xabcdef1234567890",
        token_id: 1,
        max_price: 100
      }

      assert {:ok, ^parameters} = Intent.validate_parameters(:nft, parameters)
    end

    test "validates staking parameters successfully" do
      parameters = %{
        token_address: "0xabcdef1234567890",
        amount: 1000,
        duration: 30
      }

      assert {:ok, ^parameters} = Intent.validate_parameters(:staking, parameters)
    end

    test "validates swap parameters successfully" do
      parameters = %{
        token_in: "0xabcdef1234567890",
        token_out: "0xfedcba0987654321",
        amount_in: 100,
        min_amount_out: 95
      }

      assert {:ok, ^parameters} = Intent.validate_parameters(:swap, parameters)
    end

    test "validates DAO parameters successfully" do
      parameters = %{
        dao_address: "0xabcdef1234567890",
        proposal_id: 42,
        vote_choice: :yes
      }

      assert {:ok, ^parameters} = Intent.validate_parameters(:dao, parameters)
    end

    test "returns error for missing NFT parameters" do
      parameters = %{contract_address: "0xabc"}

      assert {:error, _} = Intent.validate_parameters(:nft, parameters)
    end

    test "returns error for missing staking parameters" do
      parameters = %{token_address: "0xabc"}

      assert {:error, _} = Intent.validate_parameters(:staking, parameters)
    end

    test "returns error for missing swap parameters" do
      parameters = %{token_in: "0xabc"}

      assert {:error, _} = Intent.validate_parameters(:swap, parameters)
    end

    test "returns error for missing DAO parameters" do
      parameters = %{dao_address: "0xabc"}

      assert {:error, _} = Intent.validate_parameters(:dao, parameters)
    end

    test "returns error for unknown intent type" do
      parameters = %{test: "data"}

      assert {:error, "Unknown intent type"} = 
        Intent.validate_parameters(:unknown_type, parameters)
    end
  end
end
