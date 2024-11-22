defmodule Anoma.Node.Intents.IntentPool do
  @moduledoc """
  I am the intent pool for the Anoma node.
  m1dnight still has to write these docs.
  """

  require EventBroker.Event
  require Logger

  alias __MODULE__
  alias Anoma.Node
  alias Node.Registry
  alias Node.Transaction.Backends
  alias Anoma.RM.Intent
  alias EventBroker.Broker

  require Node.Event

  use EventBroker.DefFilter
  use TypedStruct
  use GenServer

  ############################################################
  #                           State                          #
  ############################################################

  typedstruct do
    @typedoc """
    I am the state of the intent pool.

    ### Fields
    - `:intents` - The intents in the pool.
    - `:node_id` - The ID of the Node.
    """
    field(:intents, MapSet.t(Intent.t()), default: MapSet.new())
    field(:node_id, String.t())
  end

  ############################################################
  #                    Genserver Helpers                     #
  ############################################################

  @spec start_link(any()) :: :ignore | {:error, any()} | {:ok, pid()}
  def start_link(args) do
    name = Registry.via(args[:node_id], __MODULE__)
    GenServer.start_link(__MODULE__, args, name: name)
  end

  @impl true
  def init(args) do
    Logger.debug("starting intent pool with #{inspect(args)}")
    node_id = args[:node_id]

    EventBroker.subscribe_me([
      Node.Event.node_filter(node_id),
      nullifier_filter()
    ])

    {:ok, %IntentPool{node_id: args[:node_id]}}
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @doc """
  I return the list of current intents.
  """
  @spec intents(String.t()) :: MapSet.t()
  def intents(node_id) do
    name = Registry.via(node_id, __MODULE__)
    Elixir.GenServer.call(name, :all_intents)
  end

  @doc """
  I add a new intent to the intent pool.
  """
  @spec new_intent(String.t(), any()) :: :ok
  def new_intent(node_id, intent) do
    name = Registry.via(node_id, __MODULE__)
    GenServer.cast(name, {:new_intent, intent})
  end

  @doc """
  I remove an intent from the intent pool.
  If the intent does not exist nothing happens.
  """
  @spec remove_intent(String.t(), any()) :: :ok
  def remove_intent(node_id, intent) do
    name = Registry.via(node_id, __MODULE__)
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

  @impl true
  def handle_info(
        %EventBroker.Event{
          body: %Node.Event{body: %Backends.NullifierEvent{nullifiers: set}}
        },
        state
      ) do
    {:noreply, handle_new_nullifiers(state, set)}
  end

  ############################################################
  #                 Genserver Implementation                 #
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

      EventBroker.event(
        Node.Event.new_with_body(state.node_id, {:intent_added, intent}),
        Broker
      )

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
        Node.Event.new_with_body(state.node_id, {:intent_removed, intent}),
        Broker
      )

      state = Map.update!(state, :intents, &MapSet.delete(&1, intent))
      {:ok, :removed, state}
    else
      Logger.debug("intent not removed; intent missing #{inspect(intent)}")

      {:ok, :not_present, state}
    end
  end

  @spec handle_new_nullifiers(t(), MapSet.t(binary())) :: t()
  defp handle_new_nullifiers(state, nlfs_set) do
    set_of_txs = state.intents

    new_intents =
      set_of_txs
      |> Enum.reject(fn tx ->
        tx
        |> Anoma.TransparentResource.Transaction.nullifiers()
        |> Enum.any?(&MapSet.member?(nlfs_set, &1))
      end)
      |> MapSet.new()

    %__MODULE__{state | intents: new_intents}
  end

  ############################################################
  #                         Helpers                          #
  ############################################################

  deffilter NullifierFilter do
    %EventBroker.Event{
      body: %Anoma.Node.Event{body: %Backends.NullifierEvent{}}
    } ->
      true

    _ ->
      false
  end

  defp nullifier_filter() do
    %__MODULE__.NullifierFilter{}
  end
end
