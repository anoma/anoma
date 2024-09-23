defmodule Anoma.Node.IntentPool do
  @moduledoc """
  I am the intent pool for the Anoma node.
  m1dnight still has to write these docs.
  """

  require EventBroker.Event
  alias __MODULE__
  alias EventBroker.Broker
  alias EventBroker.Event
  alias Anoma.RM.Intent

  use TypedStruct
  use GenServer

  require Logger

  ############################################################
  #                      State                               #
  ############################################################

  typedstruct do
    @typedoc """
    I am the state of the intent pool.

    ### Fields
    - `:intents` - The intents in the pool.
    """
    field(:intents, MapSet.t(Intent.t()), default: MapSet.new())
  end

  ############################################################
  #                    Genserver Helpers                     #
  ############################################################

  @spec start_link(any()) :: :ignore | {:error, any()} | {:ok, pid()}
  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: __MODULE__)
  end

  @impl true
  def init(args) do
    Logger.debug("starting intent pool with #{inspect(args)}")
    {:ok, %IntentPool{}}
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @doc """
  I return the list of current intents.
  """
  @spec intents() :: MapSet.t()
  @spec intents(GenServer.server()) :: MapSet.t()
  def intents(name \\ __MODULE__) do
    Elixir.GenServer.call(name, :all_intents)
  end

  @doc """
  I add a new intent to the intent pool.
  """
  @spec new_intent(any()) :: :ok
  @spec new_intent(GenServer.server(), any()) :: :ok
  def new_intent(name \\ __MODULE__, intent) do
    GenServer.cast(name, {:new_intent, intent})
  end

  @doc """
  I remove an intent from the intent pool.
  If the intent does not exist nothing happens.
  """
  @spec remove_intent(any()) :: :ok
  @spec remove_intent(GenServer.server(), any()) :: :ok
  def remove_intent(name \\ __MODULE__, intent) do
    GenServer.cast(name, {:remove_intent, intent})
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  @impl true
  def handle_cast({:new_intent, intent}, state) do
    {:ok, _, state} = handle_new_intent(intent, state)
    {:noreply, state}
  end

  @impl true
  def handle_cast({:remove_intent, intent}, state) do
    {:ok, _, state} = handle_remove_intent(intent, state)
    {:noreply, state}
  end

  @impl true
  def handle_call(:all_intents, _from, state) do
    {:ok, intents} = handle_all_intents(state)
    {:reply, intents, state}
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  # @doc """
  # I insert a new intent into the local state and return the updated state.
  # """
  @spec handle_new_intent(any(), t()) ::
          {:ok, :inserted, t()} | {:ok, :already_present, t()}
  defp handle_new_intent(intent, state) do
    if MapSet.member?(state.intents, intent) do
      Logger.debug("intent ignored; already present #{inspect(intent)}")

      {:ok, :already_present, state}
    else
      Logger.debug("new intent added #{inspect(intent)}")
      EventBroker.event(Broker, Event.new_with_body({:intent_added, intent}))

      state = Map.update!(state, :intents, &MapSet.put(&1, intent))
      {:ok, :inserted, state}
    end
  end

  # @doc """
  # I return all the current intents.
  # """
  @spec handle_all_intents(t()) :: {:ok, MapSet.t()}
  defp handle_all_intents(state) do
    Logger.debug("returning all intents")
    {:ok, state.intents}
  end

  # @doc """
  # I remove an intent from the local state if it exists.
  # I return the updated state and a status indicating whether the intent was removed.
  # """
  @spec handle_remove_intent(any(), t()) ::
          {:ok, :removed, t()} | {:ok, :not_present, t()}
  defp handle_remove_intent(intent, state) do
    if MapSet.member?(state.intents, intent) do
      Logger.debug("intent removed #{inspect(intent)}")

      EventBroker.event(
        Broker,
        Event.new_with_body({:intent_removed, intent})
      )

      state = Map.update!(state, :intents, &MapSet.delete(&1, intent))
      {:ok, :removed, state}
    else
      Logger.debug("intent not removed; intent missing #{inspect(intent)}")

      {:ok, :not_present, state}
    end
  end
end
