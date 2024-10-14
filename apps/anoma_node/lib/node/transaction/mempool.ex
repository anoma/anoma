defmodule Anoma.Node.Transaction.Mempool do
  @moduledoc """

  """

  alias __MODULE__
  alias Anoma.Node.Registry
  alias Anoma.Node.Transaction.{Storage, Executor, Backends}
  alias Backends.ResultEvent
  alias EventBroker.Event
  alias Executor.ExecutionEvent

  require EventBroker.Event
  require Logger

  use GenServer
  use TypedStruct

  @type vm_result :: {:ok, Noun.t()} | :error | :in_progress
  @type tx_result :: {:ok, any()} | :error | :in_progress

  typedstruct module: Tx do
    field(:tx_result, Mempool.tx_result(), default: :in_progress)
    field(:vm_result, Mempool.vm_result(), default: :in_progress)
    field(:backend, Backends.backend())
    field(:code, Noun.t())
  end

  typedstruct module: TxEvent do
    field(:id, binary())
    field(:tx, Mempool.Tx.t())
  end

  typedstruct module: ConsensusEvent do
    field(:order, list(binary()))
  end

  typedstruct module: BlockEvent do
    field(:order, list(binary()))
    field(:round, non_neg_integer())
  end

  typedstruct do
    field(:node_id, String.t())

    field(
      :transactions,
      %{binary() => Mempool.Tx.t()},
      default: %{}
    )

    field(:round, non_neg_integer(), default: 0)
  end

  def start_link(args \\ []) do
    name = Registry.name(args[:node_id], __MODULE__)
    GenServer.start_link(__MODULE__, args, name: name)
  end

  @spec init(any()) :: {:ok, Mempool.t()}
  def init(args) do
    args =
      args
      |> Keyword.validate!([
        :node_id,
        transactions: [],
        consensus: [],
        round: 0
      ])

    EventBroker.subscribe_me([
      filter_for_mempool()
    ])

    for {id, tx_w_backend} <- args[:transactions] do
      tx(tx_w_backend, id)
    end

    consensus = args[:consensus]
    round = args[:round]

    for list <- consensus do
      execute(args[:node_id], list)
    end

    {:ok, %__MODULE__{round: round, node_id: args[:node_id]}}
  end

  def terminate(reason, _state) do
    Logger.warning("mempool stopping for reason: #{inspect(reason)}")
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  def tx_dump(node_id) do
    pid = Registry.whereis(node_id, __MODULE__)
    GenServer.call(pid, :dump)
  end

  def tx(node_id, tx_w_backend) do
    tx(node_id, tx_w_backend, :crypto.strong_rand_bytes(16))
  end

  # only to be called by Logging replays directly
  def tx(node_id, tx_w_backend, id) do
    pid = Registry.whereis(node_id, __MODULE__)
    GenServer.cast(pid, {:tx, tx_w_backend, id})
  end

  # list of ids seen as ordered transactions
  @spec execute(String.t(), list(binary())) :: :ok
  def execute(node_id, ordered_list_of_txs) do
    pid = Registry.whereis(node_id, __MODULE__)
    GenServer.cast(pid, {:execute, ordered_list_of_txs})
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_call(:dump, _from, state) do
    {:reply, state.transactions |> Map.keys(), state}
  end

  def handle_call(_, _, state) do
    {:reply, :ok, state}
  end

  def handle_cast({:tx, {backend, code} = tx, tx_id}, state) do
    value = %Tx{backend: backend, code: code}

    tx_event(tx_id, value)

    Executor.launch(state.node_id, tx, tx_id)

    nstate = %Mempool{
      state
      | transactions: Map.put(state.transactions, tx_id, value)
    }

    {:noreply, nstate}
  end

  def handle_cast({:execute, id_list}, state) do
    consensus_event(id_list)
    Executor.execute(state.node_id, id_list)

    {:noreply, state}
  end

  def handle_cast(_, state) do
    {:noreply, state}
  end

  def handle_info(%Event{body: %ResultEvent{}} = e, state) do
    id = e.body.tx_id
    res = e.body.vm_result

    new_map =
      state.transactions
      |> Map.update!(id, fn tx ->
        Map.put(tx, :vm_result, res)
      end)

    new_state = %__MODULE__{state | transactions: new_map}
    {:noreply, new_state}
  end

  def handle_info(%Event{body: %ExecutionEvent{result: _}} = e, state) do
    execution_list = e.body.result
    round = state.round

    {writes, map} = process_execution(state, execution_list)

    Storage.commit(state.node_id, round, writes)

    block_event(Enum.map(execution_list, &elem(&1, 1)), round)

    {:noreply, %__MODULE__{state | transactions: map, round: round + 1}}
  end

  def handle_info(_, state) do
    {:noreply, state}
  end

  ############################################################
  #                           Helpers                        #
  ############################################################

  def block_event(id_list, round) do
    block_event =
      EventBroker.Event.new_with_body(%__MODULE__.BlockEvent{
        order: id_list,
        round: round
      })

    EventBroker.event(block_event)
  end

  def tx_event(tx_id, value) do
    tx_event =
      EventBroker.Event.new_with_body(%__MODULE__.TxEvent{
        id: tx_id,
        tx: value
      })

    EventBroker.event(tx_event)
  end

  def consensus_event(id_list) do
    consensus_event =
      EventBroker.Event.new_with_body(%__MODULE__.ConsensusEvent{
        order: id_list
      })

    EventBroker.event(consensus_event)
  end

  def worker_module_filter() do
    %EventBroker.Filters.SourceModule{module: Anoma.Node.Transaction.Backends}
  end

  def filter_for_mempool() do
    %Backends.ForMempoolFilter{}
  end

  defp process_execution(state, execution_list) do
    for {tx_res, id} <- execution_list, reduce: {[], state.transactions} do
      {lst, ex_state} ->
        {tx_struct, map} =
          Map.get_and_update!(ex_state, id, fn _ -> :pop end)

        {[Map.put(tx_struct, :tx_result, tx_res) | lst], map}
    end
  end
end
