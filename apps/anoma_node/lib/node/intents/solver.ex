defmodule Anoma.Node.Intents.Solver do
  @moduledoc """
  I am a strawman intent solver for testing purposes.

  ### Public API

  - `get_unsolved/1`
  """

  alias __MODULE__
  alias Anoma.Node
  alias Anoma.Node.Intents.IntentPool
  alias Anoma.Node.Registry
  alias Anoma.Node.Transaction.Mempool
  alias Anoma.RM.Intent
  alias EventBroker.Event

  require Logger

  use EventBroker.WithSubscription
  use GenServer
  use TypedStruct

  typedstruct do
    field(:unsolved, MapSet.t(Intent.t()), default: MapSet.new())
    field(:node_id, String.t())
    field(:enabled, boolean(), default: true)
  end

  @spec start_link([{:node_id, String.t()}]) :: GenServer.on_start()
  def start_link(args) do
    name = Registry.via(args[:node_id], __MODULE__)
    GenServer.start_link(__MODULE__, args, name: name)
  end

  @spec get_unsolved(String.t()) :: [Intent.t()]
  def get_unsolved(node_id) do
    name = Registry.via(node_id, __MODULE__)
    GenServer.call(name, :get_unsolved)
  end

  def disable(node_id) do
    name = Registry.via(node_id, __MODULE__)
    GenServer.call(name, :disable)
  end

  @impl true
  def init(args) do
    args = Keyword.validate!(args, [:node_id])
    node_id = args[:node_id]

    subscribe_to_new_intents(node_id)

    unsolved_intents =
      Enum.to_list(IntentPool.intents(node_id))

    {:ok,
     %Solver{
       unsolved: MapSet.new(unsolved_intents),
       node_id: node_id
     }}
  end

  @impl true
  def handle_call(:get_unsolved, _from, state) do
    {:reply, handle_get_unsolved(state), state}
  end

  def handle_call(:disable, _from, state) do
    state = %{state | enabled: not state.enabled}
    {:reply, state.enabled, state}
  end

  @impl true
  def handle_info(%Event{} = event, state) do
    {:noreply, handle_event(event, state)}
  end

  @spec handle_event(Event.t(), t()) :: t()
  defp handle_event(_, %{enabled: false} = state), do: state

  @spec handle_event(Event.t(), t()) :: t()
  defp handle_event(%Event{
         source_module: IntentPool,
         body: %Anoma.Node.Event{
           body: %IntentPool.Events.IntentAddSuccess{intent: intent}
         }
       }, state) do
    handle_new_intent(intent, state)
  end

  defp handle_event(event, state) do
    Logger.warning("unexpected event in solver: #{inspect(event)}")
    state
  end

  @spec handle_get_unsolved(t()) :: [Intent.t()]
  defp handle_get_unsolved(state) do
    Enum.to_list(state.unsolved)
  end

  @spec handle_new_intent(Intent.t(), t()) :: t()
  defp handle_new_intent(intent, state) do
    Logger.debug("solver received new intent: #{inspect(intent)}")
    unsolved? = intent in state.unsolved

    unless unsolved? do
      state
      |> Map.update!(:unsolved, &MapSet.put(&1, intent))
      |> do_solve()
    else
      Logger.debug("ignoring intent; already unsolved: #{unsolved?}")
      state
    end
  end

  @spec do_solve(t()) :: t()
  def do_solve(state) do
    unsolved_list = Enum.to_list(state.unsolved)
    set = solve(unsolved_list)

    unsolved =
      MapSet.filter(state.unsolved, &unsolved_reject(set, &1))

    unless Enum.empty?(set) do
      set |> Enum.reduce(&Intent.compose/2) |> submit(state.node_id)
    end

    %{state | unsolved: unsolved}
  end

  @spec solve([Intent.t()]) :: MapSet.t(Intent.t())
  def solve(intents) do
    intents
    |> subsets()
    |> Stream.drop_while(&(valid?(&1) != true))
    |> Enum.to_list()
    |> List.first([]) # returns [] if no valid subset
    |> MapSet.new()
  end

  @spec valid?([Intent.t()]) :: true | {:error, any()} | {:error, :invalid}
  def valid?([]), do: {:error, :error}

  def valid?(intents) do
    intents
    |> Enum.reduce(&Intent.compose/2)
    |> Intent.verify()
    |> case do
      true -> true
      false -> {:error, :invalid}
    end
  end

  @spec subscribe_to_new_intents(String.t()) :: :ok | String.t()
  defp subscribe_to_new_intents(node_id) do
    filter = %IntentPool.Events.IntentAddSuccessFilter{}

    EventBroker.subscribe_me([
      Node.Event.node_filter(node_id),
      filter
    ])
  end

  @spec subsets([Intent.t()]) :: Enumerable.t()
  def subsets([]), do: [[]]

  def subsets([x | xs]) do
    subsets = subsets(xs)

    Stream.map(subsets, fn subset -> [x | subset] end)
    |> Stream.concat(subsets)
  end

  @spec submit(Intent.t(), String.t()) :: :ok
  def submit(%Anoma.RM.Transparent.Transaction{} = tx, node_id) do
    tx_noun = tx |> Noun.Nounable.to_noun()
    tx_candidate = [[1, 0, [1 | tx_noun], 0 | 909], 0 | 707]
    tx_filter = [Node.Event.node_filter(node_id), %Mempool.Events.TxFilter{}]

    with_subscription tx_filter do
      Mempool.tx(
        node_id,
        {:transparent_resource, tx_candidate}
      )

      receive do
        %Event{
          body: %Node.Event{
            node_id: ^node_id,
            body: %Mempool.Events.TxEvent{
              tx: %Mempool.Tx{code: ^tx_candidate}
            }
          }
        } ->
          :ok
      after
        5000 ->
          Logger.warning("Timeout waiting for tx submission confirmation")
          :ok
      end
    end
  end

  def submit(_, _) do
    Logger.warning("Unsupported submission type provided to submit/2")
    :ok
  end

  @spec unsolved_reject(MapSet.t(Intent.t()), Intent.t()) :: boolean()
  defp unsolved_reject(solved, intent) do
    not MapSet.member?(solved, intent) and
      MapSet.disjoint?(
        solved,
        MapSet.union(
          MapSet.new(Intent.nullifiers(intent)),
          MapSet.new(Intent.commitments(intent))
        )
      )
  end
end
