defmodule Anoma.Node.Transaction.Mempool do
  @moduledoc """

  """

  alias __MODULE__
  alias Anoma.Node.Transaction.Backends
  alias Anoma.Node.Transaction.Ordering
  alias Anoma.Node.Transaction.Storage

  require EventBroker.Event

  use TypedStruct

  @type result :: {:ok, any()} | :error | :in_progress

  typedstruct module: Tx do
    field(:result, Mempool.result(), default: :in_progress)
    field(:backend, Backends.backend())
    field(:code, Noun.t())
  end

  typedstruct module: TxEvent do
    field(:id, binary())
    field(:tx, Mempool.Tx.t())
  end

  typedstruct module: ConsensusEvent do
    field(:order, list(binary()))
    field(:round, non_neg_integer())
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
    GenServer.call(__MODULE__, {:execute, ordered_list_of_txs})
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_call(:dump, _from, state) do
    {:reply, state.transactions |> Map.keys(), state}
  end

  def handle_call({:execute, id_list}, _from, state) do
    round = state.round
    tx_map = state.transactions

    consensus_event =
      EventBroker.Event.new_with_body(%__MODULE__.ConsensusEvent{
        order: id_list,
        round: round
      })

    EventBroker.event(consensus_event)

    # we should also check for id uniqueness
    with true <-
           id_list
           |> Enum.all?(fn x -> Map.has_key?(tx_map, x) end) do
      Ordering.order(id_list)

      # check which tasks were already completed and return the uncompleted list

      new_txs =
        for id <- id_list,
            reduce: tx_map do
          map ->
            unless Map.get(map, id).result == :in_progress do
              map
            else
              receive do
                %EventBroker.Event{
                  body: %Anoma.Node.Transaction.Backends.CompleteEvent{
                    tx_id: id,
                    result: res
                  }
                } ->
                  EventBroker.unsubscribe_me([
                    worker_module_filter(),
                    Ordering.tx_id_filter(id)
                  ])

                  Map.update!(map, id, fn tx ->
                    Map.put(tx, :result, res)
                  end)
              end
            end
        end

      writes = Map.take(new_txs, id_list) |> Map.values()

      Storage.commit(round, writes)

      block_event =
        EventBroker.Event.new_with_body(%__MODULE__.BlockEvent{
          order: id_list,
          round: round
        })

      EventBroker.event(block_event)

      {:reply, :ok,
       %__MODULE__{
         state
         | transactions: Map.drop(new_txs, id_list),
           round: state.round + 1
       }}
    else
      false -> {:reply, :error, state}
    end
  end

  def handle_call(_, _, state) do
    {:reply, :ok, state}
  end

  # the pid for ro backend now needs to be specified inside the backend
  def handle_cast({:tx, {backend, code} = tx, tx_id}, state) do
    value = %Tx{backend: backend, code: code}

    tx_event =
      EventBroker.Event.new_with_body(%__MODULE__.TxEvent{
        id: tx_id,
        tx: value
      })

    EventBroker.event(tx_event)

    EventBroker.subscribe_me([
      worker_module_filter(),
      Ordering.tx_id_filter(tx_id)
    ])

    Task.start(fn ->
      Backends.execute(tx, tx_id)
    end)

    nstate = %Mempool{
      state
      | transactions: Map.put(state.transactions, tx_id, value)
    }

    {:noreply, nstate}
  end

  def handle_cast(_, state) do
    {:noreply, state}
  end

  # I handle messages of worker which are done before I execute
  # if I handle info about an already cleared tx I do nothing
  def handle_info(
        %EventBroker.Event{
          body: %Anoma.Node.Transaction.Backends.CompleteEvent{
            tx_id: id,
            result: res
          }
        },
        state
      ) do
    EventBroker.unsubscribe_me([
      worker_module_filter(),
      Ordering.tx_id_filter(id)
    ])

    new_map =
      state.transactions
      |> Map.update!(id, fn tx -> Map.put(tx, :result, res) end)

    {:noreply, %__MODULE__{state | transactions: new_map}}
  end

  def handle_info(_, state) do
    {:noreply, state}
  end

  def worker_module_filter() do
    %EventBroker.Filters.SourceModule{module: Anoma.Node.Transaction.Backends}
  end
end
