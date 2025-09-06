defmodule Examples.EIntentHub do
  @moduledoc """
  I contain examples demonstrating the IntentHub functionality.

  I show how to use the IntentHub API to register, manage, and execute
  various types of intents in the Web3 ecosystem.
  """

  alias IntentHub
  alias IntentHub.Intent

  @doc """
  I demonstrate creating and managing NFT intents.

  ## Returns
  List of created NFT intents.
  """
  @spec example_nft_intents() :: [Intent.t()]
  def example_nft_intents do
    user_address = "0x1234567890abcdef"

    # Create multiple NFT intents
    nft_intents = [
      %{
        contract_address: "0xabcdef1234567890",
        token_id: 1,
        max_price: 100
      },
      %{
        contract_address: "0xabcdef1234567890",
        token_id: 2,
        max_price: 150
      },
      %{
        contract_address: "0xfedcba0987654321",
        token_id: 1,
        max_price: 200
      }
    ]

    Enum.map(nft_intents, fn parameters ->
      {:ok, intent} = IntentHub.register_intent(user_address, :nft, parameters)
      intent
    end)
  end

  @doc """
  I demonstrate creating and managing staking intents.

  ## Returns
  List of created staking intents.
  """
  @spec example_staking_intents() :: [Intent.t()]
  def example_staking_intents do
    user_address = "0x1234567890abcdef"

    staking_intents = [
      %{
        token_address: "0xabcdef1234567890",
        amount: 1000,
        duration: 30
      },
      %{
        token_address: "0xabcdef1234567890",
        amount: 2000,
        duration: 90
      }
    ]

    Enum.map(staking_intents, fn parameters ->
      {:ok, intent} = IntentHub.register_intent(user_address, :staking, parameters)
      intent
    end)
  end

  @doc """
  I demonstrate creating and managing swap intents.

  ## Returns
  List of created swap intents.
  """
  @spec example_swap_intents() :: [Intent.t()]
  def example_swap_intents do
    user_address = "0x1234567890abcdef"

    swap_intents = [
      %{
        token_in: "0xabcdef1234567890",
        token_out: "0xfedcba0987654321",
        amount_in: 100,
        min_amount_out: 95
      },
      %{
        token_in: "0xfedcba0987654321",
        token_out: "0xabcdef1234567890",
        amount_in: 50,
        min_amount_out: 48
      }
    ]

    Enum.map(swap_intents, fn parameters ->
      {:ok, intent} = IntentHub.register_intent(user_address, :swap, parameters)
      intent
    end)
  end

  @doc """
  I demonstrate creating and managing DAO intents.

  ## Returns
  List of created DAO intents.
  """
  @spec example_dao_intents() :: [Intent.t()]
  def example_dao_intents do
    user_address = "0x1234567890abcdef"

    dao_intents = [
      %{
        dao_address: "0xabcdef1234567890",
        proposal_id: 42,
        vote_choice: :yes
      },
      %{
        dao_address: "0xabcdef1234567890",
        proposal_id: 43,
        vote_choice: :no
      }
    ]

    Enum.map(dao_intents, fn parameters ->
      {:ok, intent} = IntentHub.register_intent(user_address, :dao, parameters)
      intent
    end)
  end

  @doc """
  I demonstrate intent lifecycle management.

  ## Returns
  Map containing various intent states and statistics.
  """
  @spec example_intent_lifecycle() :: %{
    created_intents: [Intent.t()],
    executed_intents: [Intent.t()],
    failed_intents: [Intent.t()],
    stats: map()
  }
  def example_intent_lifecycle do
    user_address = "0x1234567890abcdef"

    # Create various intents
    {:ok, nft_intent} =
      IntentHub.register_intent(user_address, :nft, %{
        contract_address: "0xabcdef1234567890",
        token_id: 1,
        max_price: 100
      })

    {:ok, staking_intent} =
      IntentHub.register_intent(user_address, :staking, %{
        token_address: "0xabcdef1234567890",
        amount: 1000,
        duration: 30
      })

    {:ok, swap_intent} =
      IntentHub.register_intent(user_address, :swap, %{
        token_in: "0xabcdef1234567890",
        token_out: "0xfedcba0987654321",
        amount_in: 100,
        min_amount_out: 95
      })

    # Execute some intents
    {:ok, executed_nft} = IntentHub.execute_intent(nft_intent.id)
    {:ok, executed_staking} = IntentHub.execute_intent(staking_intent.id)

    # Fail one intent
    {:ok, failed_swap} = IntentHub.update_intent_status(swap_intent.id, :failed)

    # Get statistics
    stats = IntentHub.get_intent_stats()

    %{
      created_intents: [nft_intent, staking_intent, swap_intent],
      executed_intents: [executed_nft, executed_staking],
      failed_intents: [failed_swap],
      stats: stats
    }
  end

  @doc """
  I demonstrate querying intents by various criteria.

  ## Returns
  Map containing intents grouped by different criteria.
  """
  @spec example_intent_queries() :: %{
    user_intents: [Intent.t()],
    nft_intents: [Intent.t()],
    pending_intents: [Intent.t()],
    executed_intents: [Intent.t()]
  }
  def example_intent_queries do
    user_address = "0x1234567890abcdef"

    # Create various intents
    {:ok, nft_intent} =
      IntentHub.register_intent(user_address, :nft, %{
        contract_address: "0xabcdef1234567890",
        token_id: 1,
        max_price: 100
      })

    {:ok, staking_intent} =
      IntentHub.register_intent(user_address, :staking, %{
        token_address: "0xabcdef1234567890",
        amount: 1000,
        duration: 30
      })

    {:ok, swap_intent} =
      IntentHub.register_intent(user_address, :swap, %{
        token_in: "0xabcdef1234567890",
        token_out: "0xfedcba0987654321",
        amount_in: 100,
        min_amount_out: 95
      })

    # Execute one intent
    {:ok, _executed_nft} = IntentHub.execute_intent(nft_intent.id)

    # Query intents
    user_intents = IntentHub.list_user_intents(user_address)
    nft_intents = IntentHub.list_intents_by_type(:nft)
    pending_intents = Enum.filter(user_intents, &(&1.status == :pending))
    executed_intents = Enum.filter(user_intents, &(&1.status == :executed))

    %{
      user_intents: user_intents,
      nft_intents: nft_intents,
      pending_intents: pending_intents,
      executed_intents: executed_intents
    }
  end

  @doc """
  I demonstrate priority-based intent management.

  ## Returns
  Map containing intents with different priorities.
  """
  @spec example_priority_intents() :: %{
    high_priority: [Intent.t()],
    normal_priority: [Intent.t()],
    low_priority: [Intent.t()]
  }
  def example_priority_intents do
    user_address = "0x1234567890abcdef"
    base_parameters = %{
      contract_address: "0xabcdef1234567890",
      token_id: 1,
      max_price: 100
    }

    # Create intents with different priorities
    {:ok, high_priority_intent} =
      IntentHub.register_intent(user_address, :nft, base_parameters, priority: :high)

    {:ok, normal_priority_intent} =
      IntentHub.register_intent(user_address, :nft, base_parameters, priority: :normal)

    {:ok, low_priority_intent} =
      IntentHub.register_intent(user_address, :nft, base_parameters, priority: :low)

    # Group by priority
    all_intents = [high_priority_intent, normal_priority_intent, low_priority_intent]

    %{
      high_priority: Enum.filter(all_intents, &(&1.priority == :high)),
      normal_priority: Enum.filter(all_intents, &(&1.priority == :normal)),
      low_priority: Enum.filter(all_intents, &(&1.priority == :low))
    }
  end
end
