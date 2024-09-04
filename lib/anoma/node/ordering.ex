defmodule Anoma.Node.Ordering do
  @moduledoc """
  I am the Ordering Engine.

  My main functionality is to calculate transaction ordering and keep track
  of said ordering relative to the transaction id's.

  ### Public API

  I provide the following public functionality:

  #### Ordering

  - `true_order/2`
  - `new_order/2`

  #### Reset

  - `reset/1`
  - `hard_reset/2`

  #### Blocking

  - `caller_blocking_read_id/2`

  #### Other

  - `handle_new_order/2`
  """

  alias Anoma.Node.{Router, EventLogger, Storage}
  alias Anoma.Transaction
  alias __MODULE__

  use TypedStruct
  use Router.Engine

  @typedoc """
  I am a list of ordered transactions.
  """
  @type ordered_transactions() ::
          list(Transaction.t())

  @typedoc """
  I am a type representing a key in a key-value map.
  """

  @type key() :: any()

  typedstruct do
    @typedoc """
    I am the type of the Ordering Engine.

    I have basic fields relating to upcoming ordering for the incoming
    transactions as well as mapping of transaction ID's to their orders.

    ### Fields

    - `:storage` - The address of the storage engine
    - `:next_order` - The integer referencing the order to be given to the
                      next incoming transaction.
                      Default: 1
    - `:hash_to_order` - A map of transaction ID's and their orders.
                         Default: %{}
    - `:logger` - The Logger Engine address.
    """

    field(:storage, Router.Addr.t())
    field(:next_order, non_neg_integer(), default: 1)
    field(:hash_to_order, %{key() => non_neg_integer()}, default: %{})
    field(:logger, Router.Addr.t(), enforce: false)
  end

  @doc """
  I am the initialization function for a Ordering Engine instance.

  ### Pattern-Matching Variations

  - `init(%Ordering{})` - I initialize the Engine with the given state.
  - `init(args)` - I expect a keylist with the `:logger` and `:storage`
                   keys. I then ask the Storage engine given by the keyword
                   to setup and return the appropriate initialization
                   state.
  """

  @spec init(Ordering.t()) ::
          {:ok, Ordering.t()}
  def init(%Ordering{} = state) do
    {:ok, state}
  end

  @spec init(
          list(
            {:storage, Router.Addr.t()}
            | {:logger, Router.Addr.t()}
            | any()
          )
        ) :: {:ok, Ordering.t()}
  def init(opts) do
    return = %Ordering{
      storage: opts[:storage],
      logger: opts[:logger]
    }

    # idempotent
    Storage.setup(return.storage)
    {:ok, return}
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @doc """
  I am the true order function.

  Given a transaction ID, I get the order associated with the transaction.
  I look at the map stored in the state and then check the value of said
  ID.
  """

  @spec true_order(Router.Addr.t(), any()) :: non_neg_integer() | nil
  def true_order(ordering, id) do
    Router.call(ordering, {:true_order, id})
  end

  @doc """
  I am the function dealing with new ordered transactions.

  Given a list of ordered transactions, I update the Ordering Engine state
  appropriately by changing the `:next_order` field and filling in the
  `:hash_to_order` field with new key-value pairs pairing the new ID's with
  their assigned orderings.
  """

  @spec new_order(Router.Addr.t(), ordered_transactions()) ::
          :ok
  def new_order(ordering, ordered) do
    Router.cast(ordering, {:new_order, ordered})
  end

  @doc """
  I am the Ordering Engine reset function.

  I get rid of all hot state fields of the Ordering Engine leaving only the
  Storage and Logger address intact.
  """

  @spec reset(Router.Addr.t()) :: :ok
  def reset(ordering) do
    Router.cast(ordering, :reset)
  end

  @doc """
  I hard reset the Ordering Engine.

  Similarly to `reset/1` I get rid of all hot state fields in the Ordering
  Engine. Moreover, I ask the Storage engine to ensure that the tables are
  re-launched and put a new snapshot specified by the second argument.
  """
  @spec hard_reset(Router.Addr.t(), atom()) :: :ok
  def hard_reset(ordering, initial_snapshot) do
    Router.cast(ordering, {:hard_reset, initial_snapshot})
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_call({:true_order, id}, _from, state) do
    {:reply, do_true_order(state, id), state}
  end

  def handle_cast({:new_order, trans}, _from, state) do
    {next_order, new_map} = handle_new_order(trans, state)
    log_info({:new, next_order, new_map, state.logger})

    {:noreply, %{state | next_order: next_order, hash_to_order: new_map}}
  end

  def handle_cast(:reset, _from, state) do
    storage = state.storage
    log_info({:reset, storage, state.logger})

    {:noreply, %Ordering{storage: storage, logger: state.logger}}
  end

  def handle_cast({:hard_reset, initial_snapshot}, _from, state) do
    storage = state.storage
    Storage.ensure_new(storage)
    Storage.put_snapshot(storage, initial_snapshot)

    log_info({:hard_reset, storage, initial_snapshot, state.logger})

    {:noreply, %Ordering{storage: state.storage, logger: state.logger}}
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  @spec do_true_order(Ordering.t(), non_neg_integer()) ::
          non_neg_integer() | nil
  defp do_true_order(state, id) do
    order = Map.get(state.hash_to_order, id)
    log_info({true, order, state.logger})
    order
  end

  @doc """
  I handle the new ordered transactions coming in to the Ordering Engine.

  I send read_ready messages regarding the transaction orders, then add the
  length of the incoming transaction list to the `:next_order` value,
  finally updating the `:hash_to_order` map with new key-values.

  I return a tuple where the first argument is the new proposed next order
  while the second argument is the new map of ID's to orders.
  """

  @spec handle_new_order(ordered_transactions(), t()) ::
          {non_neg_integer(), %{key() => non_neg_integer()}}
  def handle_new_order(ordered_transactions, state) do
    num_txs = length(ordered_transactions)
    log_info({:new_handle, num_txs, state.logger})

    for order <- ordered_transactions do
      log_info({:ready_handle, Transaction.addr(order), state.logger})

      Router.send_raw(
        Transaction.addr(order),
        {:read_ready, Transaction.index(order)}
      )
    end

    new_next_order = state.next_order + length(ordered_transactions)

    new_map_elements =
      Map.new(
        ordered_transactions,
        &{Transaction.id(&1), Transaction.index(&1)}
      )

    new_map = Map.merge(state.hash_to_order, new_map_elements)
    {new_next_order, new_map}
  end

  ############################################################
  #                    Caller Blocking API                   #
  ############################################################

  @doc """
  I call the blocking read functionality on a Worker request.

  I am called by the Worker, who sends me an Ordering Engine address and a
  list with head a transaction ID and a subkey for the storage key.

  I get the orders of the transactions via checking their ID keys in the
  Ordering Engine. I then return the list by interchanging the transaction
  ID with its order and call `Storage.blocking_read/2` with the
  appropriate arguments.

  ### Pattern-Matching Variations

  - `caller_blocking_read_id(ordering, [id | subkey])` - I match on the
                                                         given list to
                                                         check transaction
                                                         ID.
  """

  @spec caller_blocking_read_id(Router.Addr.t(), Noun.t()) ::
          :error | {:ok, any()}
  def caller_blocking_read_id(ordering, [id | subkey]) do
    maybe_true_order = true_order(ordering, id)
    storage = Router.Engine.get_state(ordering).storage

    read_order =
      case maybe_true_order do
        nil ->
          receive do
            {:read_ready, true_order} ->
              true_order
          end

        true_order ->
          true_order
      end

    full_key = [read_order | subkey]
    Storage.blocking_read(storage, full_key)
  end

  ############################################################
  #                     Logging Info                         #
  ############################################################

  defp log_info({true, state, logger}) do
    EventLogger.add(
      logger,
      :info,
      "Requested true order: #{inspect(state)}"
    )
  end

  defp log_info({:new, order, map, logger}) do
    EventLogger.add(logger, :info, "Requested new order.
      Next order: #{inspect(order)}. New hash: #{inspect(map)}")
  end

  defp log_info({:reset, state, logger}) do
    EventLogger.add(
      logger,
      :debug,
      "Requested reset. Storage: #{inspect(state)}"
    )
  end

  defp log_info({:hard_reset, storage, snap, logger}) do
    EventLogger.add(logger, :debug, "Requested hard reset.
      Storage: #{inspect(storage)}. Snapshot: #{inspect(snap)}")
  end

  defp log_info({:new_handle, state, logger}) do
    EventLogger.add(logger, :info, "New tx count: #{inspect(state)}")
  end

  defp log_info({:ready_handle, state, logger}) do
    EventLogger.add(
      logger,
      :info,
      "Sending read ready to: #{inspect(state)}"
    )
  end
end
