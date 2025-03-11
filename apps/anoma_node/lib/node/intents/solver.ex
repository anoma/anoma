defmodule Anoma.Node.Intents.Solver do
  @moduledoc """
  I am a strawman transaction solver for testing purposes.

  ### Public API

  I have the following public functionality:

  - `get_unsolved/1`
  """

  alias __MODULE__
  alias Anoma.Node
  alias Anoma.RM.Transaction
  alias EventBroker.Event
  alias Node.Intents.IntentPool
  alias Node.Registry
  alias Node.Transaction.Mempool

  require Logger

  use EventBroker.WithSubscription
  use GenServer
  use TypedStruct

  ############################################################
  #                    State                                 #
  ############################################################

  typedstruct do
    @typedoc """
    I hold the state for the solver process.

    ### Fields
    - `:unsolved` - The set of unsolved intents.
                    Default: MapSet.new()
    - `:node_id` - The ID of the Node to which the Solver is connected.
    """
    field(:unsolved, MapSet.t(Transaction.t()), default: MapSet.new())
    field(:node_id, String.t())
  end

  ############################################################
  #                    Genserver Helpers                     #
  ############################################################

  @doc """
  I am a start_link function of the Solver.

  I expect a keylist containing a node ID as startup argument.
  """

  @spec start_link([{:node_id, String.t()}]) :: GenServer.on_start()
  def start_link(args) do
    name = Registry.via(args[:node_id], __MODULE__)
    GenServer.start_link(__MODULE__, args, name: name)
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @doc """
  I am a function for getting unsolved intents from the solver.

  I dump the state of the solver converting the set of the unsolved intents
  into a list.
  """
  @spec get_unsolved(String.t()) :: [Transaction.t()]
  def get_unsolved(node_id) do
    name = Registry.via(node_id, __MODULE__)
    GenServer.call(name, :get_unsolved)
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  @impl true
  def init(args) do
    # set default values for the arguments
    args = Keyword.validate!(args, [:node_id])

    node_id = args[:node_id]

    # subscribe to all new transaction pool messages
    subscribe_to_new_intents(node_id)

    # fetch the unsolved intents from the transaction pool
    unsolved_intents =
      Enum.to_list(IntentPool.intents(node_id))

    state =
      %Solver{
        unsolved: MapSet.new(unsolved_intents),
        node_id: node_id
      }

    {:ok, state}
  end

  @impl true
  def handle_call(:get_unsolved, _from, state) do
    {:reply, handle_get_unsolved(state), state}
  end

  @impl true
  def handle_info(event = %Event{}, state) do
    state = handle_event(event, state)
    {:noreply, state}
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  # @doc """
  # I handle a new event coming from the event broker.
  # I am only interested in new intents.
  # """
  @spec handle_event(Event.t(), t()) :: t()
  defp handle_event(event = %Event{}, state) do
    case event do
      %Event{
        source_module: IntentPool,
        body: %Anoma.Node.Event{
          body: %IntentPool.IntentAddSuccess{transaction: transaction}
        }
      } ->
        handle_new_intent(transaction, state)

      _ ->
        Logger.warning("unexpected event in solver: #{inspect(event)}")
        state
    end
  end

  # @doc """
  # I return a list of all unsolved intents.
  # """
  @spec handle_get_unsolved(t()) :: [Transaction.t()]
  defp handle_get_unsolved(state) do
    Enum.to_list(state.unsolved)
  end

  # @doc """
  # I handle adding a new transaction.
  # I add the transaction to the list of unsolved intents, and then attempt to solve.
  # """
  @spec handle_new_intent(Transaction.t(), t()) :: t()
  defp handle_new_intent(transaction, state) do
    Logger.debug("solver received new transaction: #{inspect(transaction)}")
    unsolved? = transaction in state.unsolved

    unless unsolved? do
      new_state = %{state | unsolved: MapSet.put(state.unsolved, transaction)}
      do_solve(new_state)
    else
      Logger.debug("ignoring transaction; unsolved: #{unsolved?}")

      state
    end
  end

  ############################################################
  #                           Solver                         #
  ############################################################

  @doc """
  I am the core function responsible for solving.

  I use the `solve` function to grab the first (maximal) composable subset
  of intents from those given in the state. Then filter out whichever ones
  are present in the result and return the remaining ones.

  Given a proper transaction structure, I also submit it to the mempool using
  an appropriate wrapper.
  """

  @spec do_solve(t()) :: t()
  def do_solve(state) do
    unsolved_list = state.unsolved |> Enum.to_list()
    set = unsolved_list |> solve()

    unsolved = MapSet.filter(state.unsolved, &unsolved_reject(set, &1))

    unless Enum.empty?(set) do
      set |> Enum.reduce(&Transaction.compose/2) |> submit(state.node_id)
    end

    %{state | unsolved: unsolved}
  end

  @doc """
  Given a list of intents, I attempt to find a subset of them that can be composed
  and verified and return it as a set.

  I assume that the composition of intents is associative and commutative.
  """

  @spec solve([Transaction.t()]) ::
          MapSet.t(Transaction.t())
  def solve(intents) do
    intents
    |> subsets()
    |> Stream.drop_while(&(valid?(&1) != true))
    |> Enum.to_list()
    |> List.first([])
    |> MapSet.new()
  end

  @doc """
  I check if a list of intents is valid by composing them and verifying if they satisfy
  the Transaction.valid? predicate.
  """

  @spec valid?([Transaction.t()]) :: true | {:error, any()}
  def valid?([]), do: {:error, :error}

  def valid?(intents) do
    intents
    |> Enum.reduce(&Transaction.compose/2)
    |> Transaction.verify()
  end

  ############################################################
  #                           Helpers                        #
  ############################################################

  # @doc """
  # I subscribe this process to the transaction pool events.
  # """
  @spec subscribe_to_new_intents(String.t()) :: :ok | String.t()
  defp subscribe_to_new_intents(node_id) do
    filter = %IntentPool.IntentAddSuccessFilter{}

    EventBroker.subscribe_me([
      Node.Event.node_filter(node_id),
      filter
    ])
  end

  @doc """
  I generate all possible subsets of a given list of elements as a stream.
  """
  @spec subsets([Transaction.t()]) :: Enumerable.t()
  def subsets([]), do: [[]]

  def subsets([x | xs]) do
    subsets = subsets(xs)

    Stream.map(subsets, fn subset -> [x | subset] end)
    |> Stream.concat(subsets)
  end

  @doc """
  I provide the submit functionality for the solver.

  Given something solved, I submit it to the Mempool with the provided ID.

  ### Pattern-Matching Variations

  - `submit(%TransparentResource.Transaction{}, node_id)` - I wrap a transaction in trivial
                                                            core and send it to the Mempool.
  - `submit(any, node_id)` - I do nothing
  """

  @spec submit(Transaction.t(), String.t()) :: :ok
  def submit(tx = %Anoma.TransparentResource.Transaction{}, node_id) do
    tx_noun = tx |> Noun.Nounable.to_noun()
    tx_candidate = [[1, 0, [1 | tx_noun], 0 | 909], 0 | 707]
    tx_filter = [Node.Event.node_filter(node_id), %Mempool.TxFilter{}]

    with_subscription [tx_filter] do
      Mempool.tx(
        node_id,
        {:transparent_resource, tx_candidate}
      )

      receive do
        %EventBroker.Event{
          body: %Node.Event{
            node_id: ^node_id,
            body: %Mempool.TxEvent{
              tx: %Mempool.Tx{backend: _, code: ^tx_candidate}
            }
          }
        } ->
          :ok
      end
    end
  end

  def submit(_, _) do
  end

  @spec unsolved_reject(MapSet.t(Transaction.t()), Transaction.t()) :: bool()
  defp unsolved_reject(solved, transaction) do
    not MapSet.member?(solved, transaction) and
      MapSet.disjoint?(
        solved,
        MapSet.union(
          Transaction.nullifiers(transaction),
          Transaction.commitments(transaction)
        )
      )
  end
end
