defmodule Anoma.Node.Transaction.Mempool do
  @moduledoc """

  """

  alias __MODULE__
  alias Anoma.Node.Transaction.{Storage, Executor, Backends}

  require EventBroker.Event

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
    field(
      :transactions,
      %{binary() => Mempool.Tx.t()},
      default: %{}
    )

    field(:round, non_neg_integer(), default: 0)
  end

  def start_link(default) do
    GenServer.start_link(__MODULE__, default, name: Mempool)
  end

  @spec init(any()) :: {:ok, Mempool.t()}
  def init(_arg) do
    EventBroker.subscribe_me([
      filter_for_mempool()
    ])

    {:ok, %__MODULE__{}}
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  def tx_dump() do
    GenServer.call(__MODULE__, :dump)
  end

  def tx(tx_w_backend) do
    tx(tx_w_backend, :crypto.strong_rand_bytes(16))
  end

  # only to be called by Logging replays directly
  def tx(tx_w_backend, id) do
    GenServer.cast(__MODULE__, {:tx, tx_w_backend, id})
  end

  # list of ids seen as ordered transactions
  @spec execute(list(binary())) :: :ok
  def execute(ordered_list_of_txs) do
    GenServer.cast(__MODULE__, {:execute, ordered_list_of_txs})
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

    Executor.launch(tx, tx_id)

    nstate = %Mempool{
      state
      | transactions: Map.put(state.transactions, tx_id, value)
    }

    {:noreply, nstate}
  end

  def handle_cast({:execute, id_list}, state) do
    consensus_event(id_list)
    Executor.execute(id_list)

    {:noreply, state}
  end

  def handle_cast(_, state) do
    {:noreply, state}
  end

  def handle_info(
        %EventBroker.Event{
          body: %Backends.ResultEvent{
            tx_id: id,
            vm_result: res
          }
        },
        state
      ) do
    new_map =
      state.transactions
      |> Map.update!(id, fn tx -> Map.put(tx, :vm_result, res) end)

    {:noreply, %__MODULE__{state | transactions: new_map}}
  end

  def handle_info(
        %EventBroker.Event{
          body: %Executor.ExecutionEvent{
            result: execution_list
          }
        },
        state
      ) do
    round = state.round

    {writes, map} = process_execution(state, execution_list)

    Storage.commit(round, writes)

    block_event(Enum.map(execution_list, &elem(&1, 1)), round)

    {:noreply, %__MODULE__{state | transactions: map, round: round + 1}}
  end

  def handle_info(_, state) do
    {:noreply, state}
  end

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
