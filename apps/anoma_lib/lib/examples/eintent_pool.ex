defmodule Examples.IntentPool do
  @moduledoc """
  I contain several examples on how to run the intent pool.
  """

  require ExUnit.Assertions
  import ExUnit.Assertions

  alias Anoma.Node.IntentPool
  alias Anoma.ShieldedResource.ShieldedTransaction
  alias Anoma.RM.Intent
  alias Anoma.RM.DumbIntent

  ############################################################
  #                  Initialization                          #
  ############################################################

  @doc """
  I start a new intent pool and return its process id.
  """
  @spec initialization() :: %{intent_pool: %{pid: pid(), name: atom()}}
  def initialization() do
    name =
      Enum.shuffle(?a..?z) |> Enum.take(20) |> to_string |> String.to_atom()

    {:ok, pid} = GenServer.start_link(Anoma.Node.IntentPool, %{}, name: name)

    %{intent_pool: %{pid: pid, name: name}}
  end

  @doc """
  I cleanup the processes started for the examples.
  """
  @spec cleanup(%{intent_pool: %{pid: pid(), name: atom()}}) :: :ok
  def cleanup(context) do
    :ok = GenServer.stop(context.intent_pool.pid, :normal)
  end

  ############################################################
  #                           Scenarios                      #
  ############################################################

  @doc """
  I check that the intent pool returns an empty map of intents when its started.
  """
  @spec list_intents() :: :ok
  def list_intents() do
    # setup
    context = initialization()
    intent_pool = context.intent_pool.name

    # the intent list is empty when the pool is first started
    assert MapSet.new() == IntentPool.intents(intent_pool)

    # cleanup
    cleanup(context)
  end

  @doc """
  I check that when an intent is added to the pool, is is present in the mapset.
  """
  @spec add_intent() :: :ok
  def add_intent() do
    # setup
    context = initialization()
    intent_pool = context.intent_pool.pid

    # add an intent to the pool
    intent = %DumbIntent{}
    IntentPool.new_intent(intent_pool, intent)

    # the intent will be present in the mapset.
    assert Enum.count(IntentPool.intents(intent_pool)) == 1

    # cleanup
    cleanup(context)
  end

  @doc """
  I add and remove an intent from the pool and ensure that it's actually gone.
  """
  @spec remove_intent() :: :ok
  def remove_intent() do
    # setup
    context = initialization()
    intent_pool = context.intent_pool.pid

    # add an intent to the pool
    intent = %DumbIntent{}
    IntentPool.new_intent(intent_pool, intent)

    # the intent will be present in the mapset.
    assert Enum.count(IntentPool.intents(intent_pool)) == 1

    # remove the intent from the pool
    IntentPool.remove_intent(intent_pool, intent)

    # the intent will be removed from the mapset.
    assert Enum.count(IntentPool.intents(intent_pool)) == 0

    # cleanup
    cleanup(context)
  end
end
