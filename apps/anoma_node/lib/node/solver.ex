defmodule Anoma.Node.Solver do
  @moduledoc """
  I am a strawman intent solver for testing purposes.

  ### Public API

  I have the following public functionality:

  - `get_solved/0`
  - `send/1`
  """

  use TypedStruct
  use GenServer

  alias __MODULE__
  alias Anoma.Node.IntentPool
  alias Anoma.RM.Intent
  alias EventBroker.Event
  alias EventBroker.Filters

  require Logger

  ############################################################
  #                    State                                 #
  ############################################################

  typedstruct do
    @typedoc """
    I hold the state for the solver process.

    ### Fields
    - `:unsolved`        - The set of unsolved intents.
    - `:solved`          - The set of solved intents.
    - `:intent_pool`     - The intent pool process name.
    """
    field(:unsolved, MapSet.t(Intent.t()), default: MapSet.new())
    field(:solved, MapSet.t(Intent.t()), default: MapSet.new())
    field(:intent_pool, GenServer.server())
  end

  ############################################################
  #                    Genserver Helpers                     #
  ############################################################

  @doc """
  I create a new solver process .
  """
  def start_link(args) do
    GenServer.start_link(__MODULE__, args)
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @doc """
  I return all the solved intents.
  """
  @spec get_solved :: [Intent.t()]
  @spec get_solved(GenServer.server()) :: [Intent.t()]
  def get_solved(solver \\ __MODULE__) do
    GenServer.call(solver, :get_solved)
  end

  @doc """
  I return all the unsolved intents.
  """
  @spec get_unsolved :: [Intent.t()]
  @spec get_unsolved(GenServer.server()) :: [Intent.t()]
  def get_unsolved(solver \\ __MODULE__) do
    GenServer.call(solver, :get_unsolved)
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  @impl true
  def init(args) do
    # set default values for the arguments
    args = Keyword.validate!(args, intent_pool: __MODULE__)

    # subscribe to all new intent pool messages
    subscribe_to_new_intents()

    # fetch the unsolved intents from the intent pool
    unsolved_intents = Enum.to_list(IntentPool.intents(args[:intent_pool]))

    state =
      %Solver{
        unsolved: MapSet.new(unsolved_intents),
        intent_pool: args[:intent_pool]
      }

    {:ok, state}
  end

  @impl true
  def handle_call(:get_solved, _from, state) do
    {:reply, handle_get_solved(state), state}
  end

  @impl true
  def handle_call(:get_unsolved, _from, state) do
    {:reply, handle_get_unsolved(state), state}
  end

  @impl true
  def handle_info(%Event{} = event, state) do
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
  defp handle_event(%Event{} = event, state) do
    case event do
      %Event{source_module: IntentPool, body: {:intent_added, intent}} ->
        handle_new_intent(intent, state)

      _ ->
        Logger.warning("unexpected event in solver: #{inspect(event)}")
        state
    end
  end

  # @doc """
  # I return a list of all solved intents.
  # """
  @spec handle_get_solved(t()) :: [Intent.t()]
  defp handle_get_solved(state) do
    Enum.to_list(state.solved)
  end

  # @doc """
  # I return a list of all unsolved intents.
  # """
  @spec handle_get_unsolved(t()) :: [Intent.t()]
  defp handle_get_unsolved(state) do
    Enum.to_list(state.unsolved)
  end

  # @doc """
  # I handle adding a new intent.
  # I add the intent to the list of unsolved intents, and then attempt to solve.
  # """
  @spec handle_new_intent(Intent.t(), t()) :: t()
  defp handle_new_intent(intent, state) do
    Logger.debug("solver received new intent: #{inspect(intent)}")
    unsolved? = intent in state.unsolved
    solved? = intent in state.solved

    if not unsolved? and not solved? do
      new_state = %{state | unsolved: MapSet.put(state.unsolved, intent)}
      do_solve(new_state)
    else
      Logger.debug(
        "ignoring intent; unsolved: #{unsolved?}, solved: #{solved?}"
      )

      state
    end
  end

  ############################################################
  #                           Solver                         #
  ############################################################

  # @doc """
  # I try and solve the intents currently in my state.
  # If I can solve some of them, I add them to the solved pool.
  # """
  def do_solve(state) do
    {solved, unsolved} =
      case solve(state.unsolved) do
        nil ->
          {state.solved, state.unsolved}

        new_solves ->
          new_solves = MapSet.union(state.solved, new_solves)
          new_unsolveds = MapSet.difference(state.unsolved, new_solves)
          {new_solves, new_unsolveds}
      end

    %{state | solved: solved, unsolved: unsolved}
  end

  # @doc """
  # Given a list of intents, I attempt to find a subset of them that can be composed
  # and verified.
  # I return a list of all intents that can be solved when composed.
  #
  # I assume that the composition of intents is associative and commutative.
  # """
  @spec solve([Intent.t()] | MapSet.t(Intent.t())) ::
          MapSet.t(Intent.t()) | nil
  def solve(intents) do
    intents
    |> Enum.to_list()
    |> subsets()
    |> Stream.drop_while(&(not valid?(&1)))
    |> Stream.take(1)
    |> Enum.to_list()
    |> Enum.map(&MapSet.new/1)
    |> List.first()
  end

  @doc """
  I check if a list of intents is valid by composing them and verifying if they satisfy
  the Intent.valid? predicate.
  """
  @spec valid?([Intent.t()]) :: boolean()
  def valid?([]), do: false

  def valid?(intents) do
    intents
    |> Enum.reduce(&Intent.compose/2)
    |> Intent.verify()
  end

  ############################################################
  #                           Helpers                        #
  ############################################################

  # @doc """
  # I subscribe this process to the intent pool events.
  # """
  @spec subscribe_to_new_intents() :: :ok | String.t()
  defp subscribe_to_new_intents() do
    filter = %Filters.SourceModule{module: IntentPool}
    EventBroker.subscribe_me(EventBroker.Registry, [filter])
  end

  @doc """
  I generate all possible subsets of a given list of elements as a stream.
  """
  @spec subsets(Enumerable.t()) :: Enumerable.t()
  def subsets([]), do: [[]]

  def subsets([x | xs]) do
    subsets = subsets(xs)

    Stream.map(subsets, fn subset -> [x | subset] end)
    |> Stream.concat(subsets)
  end
end
