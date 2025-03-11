defmodule Anoma.Node.Intents.IntentPool do
  @moduledoc """
  I am the transaction pool for the Anoma node.
  m1dnight still has to write these docs.
  """

  alias __MODULE__
  alias Anoma.Node
  alias Anoma.Node.Tables
  alias Anoma.RM.Transaction
  alias EventBroker.Broker
  alias Node.Registry
  alias Node.Transaction.Backends

  require EventBroker.Event
  require Logger
  require Node.Event

  use EventBroker.DefFilter
  use GenServer
  use TypedStruct

  typedstruct enforce: true, module: IntentAddSuccess do
    @typedoc """
    I am an event specifying that an transaction has been submitted succesfully.

    ### Fields
    - `:transaction` - The transaction added.
    """
    field(:transaction, Transaction.t())
  end

  typedstruct enforce: true, module: IntentAddError do
    @typedoc """
    I am an event specifying that an transaction submission has failed alongside
    with a reason.

    ### Fields
    - `:transaction` - The transaction submitted.
    - `:reason` - The reason why it was rejected from the pool.
    """
    field(:transaction, Transaction.t())
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
    I am the state of the transaction pool.

    ### Fields
    - `:intents` - The intents in the pool.
    - `:node_id` - The ID of the Node.
    - `:nlfs_set` - The set of known nullifiers.
    - `:cms_set` - The set of known commitments.
    """
    field(:intents, MapSet.t(Transaction.t()), default: MapSet.new())
    field(:node_id, String.t())
    field(:nlfs_set, MapSet.t(binary()), default: MapSet.new())
    field(:cms_set, MapSet.t(binary()), default: MapSet.new())
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
    Logger.debug("starting transaction pool with #{inspect(args)}")

    args =
      args
      |> Keyword.validate!([
        :node_id,
        intents: MapSet.new([]),
        nlfs_set: MapSet.new([]),
        cms_set: MapSet.new([]),
        rocks: false
      ])

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
       cms_set: args[:cms_set]
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
  I add a new transaction to the transaction pool.
  """
  @spec new_intent(String.t(), any()) :: :ok
  def new_intent(node_id, transaction) do
    name = Registry.via(node_id, __MODULE__)
    GenServer.cast(name, {:new_intent, transaction})
  end

  @doc """
  I remove an transaction from the transaction pool.
  If the transaction does not exist nothing happens.
  """
  @spec remove_intent(String.t(), any()) :: :ok
  def remove_intent(node_id, transaction) do
    name = Registry.via(node_id, __MODULE__)
    GenServer.cast(name, {:remove_intent, transaction})
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  @impl true
  def handle_cast({:new_intent, transaction}, state) do
    {:ok, _, state} = handle_new_intent(transaction, state)
    {:noreply, state}
  end

  @impl true
  def handle_cast({:remove_intent, transaction}, state) do
    {:ok, _, state} = handle_remove_intent(transaction, state)
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
  # I insert a new transaction into the local state and return the updated state.
  # I return the current state if the transaction is already present.
  # I return the current state if any nullifier of the transaction is already known.
  # """
  @spec handle_new_intent(any(), t()) ::
          {:ok, :inserted, t()}
          | {:ok,
             :already_present | :nullifiers_present | :commitments_present,
             t()}
  defp handle_new_intent(transaction, state) do
    with :ok <- validate_intent_uniqueness(transaction, state),
         :ok <- validate_nullifier_uniqueness(transaction, state.nlfs_set),
         :ok <- validate_commitment_uniqueness(transaction, state.cms_set) do
      table = state.table

      :mnesia.transaction(fn ->
        res =
          case :mnesia.read(table, "intents") do
            [] -> MapSet.new()
            [{^table, "intents", res}] -> res
          end

        :mnesia.write({table, "intents", MapSet.put(res, transaction)})
      end)

      {:ok, :inserted, add_intent!(transaction, state)}
    else
      {:error, reason} -> handle_error(transaction, reason, state)
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
  # I remove an transaction from the local state if it exists.
  # I return the updated state and a status indicating whether the transaction was removed.
  # """
  @spec handle_remove_intent(any(), t()) ::
          {:ok, :removed, t()} | {:ok, :not_present, t()}
  defp handle_remove_intent(transaction, state) do
    if MapSet.member?(state.intents, transaction) do
      Logger.debug("transaction removed #{inspect(transaction)}")

      EventBroker.event(
        Node.Event.new_with_body(
          state.node_id,
          {:intent_removed, transaction}
        ),
        Broker
      )

      state = Map.update!(state, :intents, &MapSet.delete(&1, transaction))
      {:ok, :removed, state}
    else
      Logger.debug(
        "transaction not removed; transaction missing #{inspect(transaction)}"
      )

      {:ok, :not_present, state}
    end
  end

  @spec handle_new_state(t(), EventBroker.Event.t()) :: t()
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

  defp validate_intent_uniqueness(transaction, state) do
    if MapSet.member?(state.intents, transaction) do
      Logger.debug(
        "transaction ignored; already present #{inspect(transaction)}"
      )

      {:error, :already_present}
    else
      :ok
    end
  end

  defp validate_nullifier_uniqueness(transaction, nlfs_set) do
    unless MapSet.disjoint?(Transaction.nullifiers(transaction), nlfs_set) do
      Logger.debug(
        "transaction ignored; uses already nullified resources #{inspect(transaction)}"
      )

      {:error, :nullifiers_present}
    else
      :ok
    end
  end

  defp validate_commitment_uniqueness(transaction, cms_set) do
    unless MapSet.disjoint?(Transaction.commitments(transaction), cms_set) do
      Logger.debug(
        "transaction ignored; uses already created resources #{inspect(transaction)}"
      )

      {:error, :commitments_present}
    else
      :ok
    end
  end

  defp add_intent!(transaction, state) do
    Logger.debug("new transaction added #{inspect(transaction)}")

    EventBroker.event(
      Node.Event.new_with_body(state.node_id, %__MODULE__.IntentAddSuccess{
        transaction: transaction
      }),
      Broker
    )

    Map.update!(state, :intents, &MapSet.put(&1, transaction))
  end

  defp handle_error(transaction, reason, state) do
    EventBroker.event(
      Node.Event.new_with_body(state.node_id, %__MODULE__.IntentAddError{
        transaction: transaction,
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
        MapSet.union(Transaction.nullifiers(&1), Transaction.commitments(&1))
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
end
