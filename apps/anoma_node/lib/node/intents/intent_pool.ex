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

  typedstruct enforce: true, module: IntentAddSuccess do
    @typedoc """
    I am an event specifying that an intent has been submitted succesfully.

    ### Fields
    - `:intent` - The intent added.
    """
    field(:intent, Intent.t())
  end

  typedstruct enforce: true, module: IntentAddError do
    @typedoc """
    I am an event specifying that an intent submission has failed alongside
    with a reason.

    ### Fields
    - `:intent` - The intent submitted.
    - `:reason` - The reason why it was rejected from the pool.
    """
    field(:intent, Intent.t())
    field(:reason, String.t())
  end

  deffilter IntentAddSuccessFilter do
    %EventBroker.Event{
      body: %Node.Event{body: %IntentPool.IntentAddSuccess{}}
    } ->
      true

    _ ->
      false
  end

  deffilter IntentAddErrorFilter do
    %EventBroker.Event{body: %Node.Event{body: %IntentPool.IntentAddError{}}} ->
      true

    _ ->
      false
  end

  ############################################################
  #                           State                          #
  ############################################################

  typedstruct do
    @typedoc """
    I am the state of the intent pool.

    ### Fields
    - `:intents` - The intents in the pool.
    - `:node_id` - The ID of the Node.
    - `:nlfs_set` - The set of known nullifiers.
    - `:cms_set` - The set of known commitments.
    - `:table` - The table name for storing intents.
    """
    field(:intents, MapSet.t(Intent.t()), default: MapSet.new())
    field(:node_id, String.t())
    field(:nlfs_set, MapSet.t(binary()), default: MapSet.new())
    field(:cms_set, MapSet.t(binary()), default: MapSet.new())
    field(:table, atom())
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

    args =
      args
      |> Keyword.validate!([
        :node_id,
        intents: MapSet.new([]),
        nlfs_set: MapSet.new([]),
        cms_set: MapSet.new([]),
        rocks: false,
        table: __MODULE__.Intents
      ])

    table =
      String.to_atom("#{args[:table]}_#{:erlang.phash2(args[:node_id])}")

    rocks_opt = args[:rocks] |> Anoma.Utility.rock_opts()
    :mnesia.create_table(table, rocks_opt ++ [attributes: [:type, :body]])

    node_id = args[:node_id]

    EventBroker.subscribe_me([
      Node.Event.node_filter(node_id),
      trm_filter()
    ])

    intents =
      reject_intents(
        args[:intents],
        MapSet.union(args[:nlfs_set], args[:cms_set])
      )

    {:ok,
     %IntentPool{
       node_id: node_id,
       intents: intents,
       nlfs_set: args[:nlfs_set],
       cms_set: args[:cms_set],
       table: table
     }}
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
        e = %EventBroker.Event{
          body: %Node.Event{body: %Backends.TRMEvent{}}
        },
        state
      ) do
    {:noreply, handle_new_state(state, e)}
  end

  ############################################################
  #                 Genserver Implementation                 #
  ############################################################

  # @doc """
  # I insert a new intent into the local state and return the updated state.
  # I return the current state if the intent is already present.
  # I return the current state if any nullifier of the intent is already known.
  # """
  @spec handle_new_intent(any(), t()) ::
          {:ok, :inserted, t()}
          | {:ok,
             :already_present | :nullifiers_present | :commitments_present,
             t()}
  defp handle_new_intent(intent, state) do
    with :ok <- validate_intent_uniqueness(intent, state),
         :ok <- validate_nullifier_uniqueness(intent, state.nlfs_set),
         :ok <- validate_commitment_uniqueness(intent, state.cms_set) do
      table = state.table

      :mnesia.transaction(fn ->
        res =
          case :mnesia.read(table, "intents") do
            [] -> MapSet.new()
            [{^table, "intents", res}] -> res
          end

        :mnesia.write({table, "intents", MapSet.put(res, intent)})
      end)

      {:ok, :inserted, add_intent!(intent, state)}
    else
      {:error, reason} -> handle_error(intent, reason, state)
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

  @spec handle_new_state(t(), %EventBroker.Event{}) :: t()
  defp handle_new_state(state, %EventBroker.Event{
         body: %Node.Event{
           body: %Backends.TRMEvent{
             nullifiers: nlfs_set,
             commitments: cms_set
           }
         }
       }) do
    new_intents =
      reject_intents(state.intents, MapSet.union(nlfs_set, cms_set))

    new_nlfs_set = MapSet.union(state.nlfs_set, nlfs_set)
    new_cms_set = MapSet.union(state.cms_set, cms_set)

    %__MODULE__{
      state
      | intents: new_intents,
        nlfs_set: new_nlfs_set,
        cms_set: new_cms_set
    }
  end

  ############################################################
  #                         Helpers                          #
  ############################################################

  defp validate_intent_uniqueness(intent, state) do
    if MapSet.member?(state.intents, intent) do
      Logger.debug("intent ignored; already present #{inspect(intent)}")
      {:error, :already_present}
    else
      :ok
    end
  end

  defp validate_nullifier_uniqueness(intent, nlfs_set) do
    unless MapSet.disjoint?(Intent.nullifiers(intent), nlfs_set) do
      Logger.debug(
        "intent ignored; uses already nullified resources #{inspect(intent)}"
      )

      {:error, :nullifiers_present}
    else
      :ok
    end
  end

  defp validate_commitment_uniqueness(intent, cms_set) do
    unless MapSet.disjoint?(Intent.commitments(intent), cms_set) do
      Logger.debug(
        "intent ignored; uses already created resources #{inspect(intent)}"
      )

      {:error, :commitments_present}
    else
      :ok
    end
  end

  defp add_intent!(intent, state) do
    Logger.debug("new intent added #{inspect(intent)}")

    EventBroker.event(
      Node.Event.new_with_body(state.node_id, %__MODULE__.IntentAddSuccess{
        intent: intent
      }),
      Broker
    )

    Map.update!(state, :intents, &MapSet.put(&1, intent))
  end

  defp handle_error(intent, reason, state) do
    EventBroker.event(
      Node.Event.new_with_body(state.node_id, %__MODULE__.IntentAddError{
        intent: intent,
        reason: reason
      }),
      Broker
    )

    {:ok, reason, state}
  end

  def reject_intents(intents, set) do
    intents
    |> Enum.filter(
      &MapSet.disjoint?(
        set,
        MapSet.union(Intent.nullifiers(&1), Intent.commitments(&1))
      )
    )
    |> MapSet.new()
  end

  deffilter TRMFilter do
    %EventBroker.Event{
      body: %Anoma.Node.Event{body: %Backends.TRMEvent{}}
    } ->
      true

    _ ->
      false
  end

  defp trm_filter() do
    %__MODULE__.TRMFilter{}
  end

  @doc """
  I am the name of the unsolved table.
  Given a Node ID, I create an appropriately named table for storing
  unsolved intents.
  """

  @spec table_name(String.t()) :: atom()
  def table_name(node_id) do
    String.to_atom("#{__MODULE__.Intents}_#{:erlang.phash2(node_id)}")
  end
end
