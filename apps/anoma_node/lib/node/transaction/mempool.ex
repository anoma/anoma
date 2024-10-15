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
  @typep startup_options() :: {:node_id, String.t()}

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

  @spec start_link([startup_options()]) :: GenServer.on_start()
  def start_link(args \\ []) do
    name = Registry.via(args[:node_id], __MODULE__)
    GenServer.start_link(__MODULE__, args, name: name)
  end

  @spec init([startup_options()]) :: {:ok, Mempool.t()}
  def init(args) do
    Process.set_label(__MODULE__)

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

    {:ok, %__MODULE__{node_id: args[:node_id]}}
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @spec tx_dump(String.t()) :: [Mempool.Tx.t()]
  def tx_dump(node_id) do
    GenServer.call(Registry.via(node_id, __MODULE__), :dump)
  end

  @spec tx(String.t(), {Backends.backend(), Noun.t()}) :: :ok
  def tx(node_id, tx_w_backend) do
    tx(node_id, tx_w_backend, :crypto.strong_rand_bytes(16))
  end

  # only to be called by Logging replays directly
  @spec tx(String.t(), {Backends.backend(), Noun.t()}, binary()) :: :ok
  def tx(node_id, tx_w_backend, id) do
    GenServer.cast(Registry.via(node_id, __MODULE__), {:tx, tx_w_backend, id})
  end

  # list of ids seen as ordered transactions
  @spec execute(String.t(), list(binary())) :: :ok
  def execute(node_id, ordered_list_of_txs) do
    GenServer.cast(
      Registry.via(node_id, __MODULE__),
      {:execute, ordered_list_of_txs}
    )
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

  @spec process_execution(t(), [{:ok | :error, binary()}]) ::
          {[Mempool.Tx.t()], %{binary() => Mempool.Tx.t()}}
  defp process_execution(state, execution_list) do
    for {tx_res, id} <- execution_list, reduce: {[], state.transactions} do
      {lst, ex_state} ->
        {tx_struct, map} =
          Map.get_and_update!(ex_state, id, fn _ -> :pop end)

        {[Map.put(tx_struct, :tx_result, tx_res) | lst], map}
    end
  end
end
