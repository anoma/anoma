defmodule Anoma.Node.Transaction.Mempool do
  @moduledoc """
  I am the Mempool Engine.

  I posess the core functionality to submit new transactions, execute
  incoming consensus, and dump current transactions. Alongside that, I
  store all currently running transactions as well as their intermediate
  VM results.

  As the main point of user-input, I also send the events needed for
  replays.

  All transactions are assumed to come in the form of {backend, noun}.

  All consensus is assumed to come in a form of an orered list of binaries.

  ### Public API

  I provide the following public functionality:

  - `tx_dump/1`
  - `execute/2`
  - `tx/2`
  - `tx/3`
  """

  alias __MODULE__
  alias Anoma.Node
  alias Anoma.Node.Events
  alias Anoma.Node.Registry
  alias Anoma.Node.Transaction.Backends
  alias Anoma.Node.Transaction.Executor
  alias Anoma.Node.Transaction.Storage

  require Logger
  require Node.Event

  use EventBroker.DefFilter
  use GenServer
  use TypedStruct

  ############################################################
  #                       Types                              #
  ############################################################

  @typedoc """
  I am the type of the Nock VM result.
  """
  @type vm_result :: {:ok, Noun.t()} | :error | :in_progress

  @typedoc """
  I am the type of the transaction result.
  """
  @type tx_result :: {:ok, any()} | :error | :in_progress

  @typedoc """
  Type of the arguments the mempool genserver expects
  """
  @type args_t ::
          [
            node_id: String.t(),
            transactions: [{binary, {Backends.backend(), Noun.t()}}],
            consensus: [[binary()]],
            round: non_neg_integer()
          ]
          | [node_id: String.t()]

  ############################################################
  #                         State                            #
  ############################################################

  typedstruct module: Tx do
    @typedoc """
    I am the type of a transaction as stores in the Mempool.

    I store all information about transaction results, backend it uses, as
    well as the Nockma represented code.

    ### Fields

    - `:tx_result` - The transaction execution result.
                     Default: `:in_progress`
    - `:vm_result` - The Nock VM result of the transaction code.
                     Default: `:in_progress`
    - `:backend` - The backend for the transaction.
    - `:code` - The Nockma transaction code to be executed.
    """

    field(:tx_result, Mempool.tx_result(), default: :in_progress)
    field(:vm_result, Mempool.vm_result(), default: :in_progress)
    field(:backend, Backends.backend())
    field(:code, Noun.t())
  end

  typedstruct do
    @typedoc """
    I am the type of the Mempool Engine.

    I contain the core information for the mempool functionality, storing the
    node ID for which the Mempool is launched, a map of transactions with
    their IDs, as well as the most recent block round.

    ### Fields

    - `:node_id` - The ID of the Node to which a Mempool instantiation is
                   is bound.
    - `:transactions` - A map with keys being the binary IDs of launched
                        transactions and values the corresponding
                        transaction data. See `Tx.t()`
                        Default: %{}
    - `:round` - The round of the next block to be created.
                 Default: 0
    """
    field(:node_id, String.t())

    field(
      :transactions,
      %{binary() => Mempool.Tx.t()},
      default: %{}
    )

    field(:round, non_neg_integer(), default: 0)
  end

  deffilter TxFilter do
    %EventBroker.Event{body: %Node.Event{body: %Events.TxEvent{}}} ->
      true

    _ ->
      false
  end

  deffilter ConsensusFilter do
    %EventBroker.Event{body: %Node.Event{body: %Events.ConsensusEvent{}}} ->
      true

    _ ->
      false
  end

  deffilter BlockFilter do
    %EventBroker.Event{body: %Node.Event{body: %Events.BlockEvent{}}} ->
      true

    _ ->
      false
  end

  ############################################################
  #                    Genserver Helpers                     #
  ############################################################

  @doc """
  I am the start_link function for the Mempool Engine.

  I register the mempool with supplied node ID provided by the
  arguments.
  """

  @spec start_link(args_t()) :: GenServer.on_start()
  def start_link(args \\ []) do
    name = Registry.via(args[:node_id], __MODULE__)
    GenServer.start_link(__MODULE__, args, name: name)
  end

  @impl true
  @doc """
  I am the initialization function for the Mempool Engine.

  I assume that my arguments come with keywords specifying the node id,
  alongside transactions, pending orders, and a block round.

  If any transactions are provided upon startup, I ask the Mempool to
  execute them with a particular ID.

  If any orders are provided, I also launch them afterwards in the order
  specified.

  Afterwards, I initialize the Mempool with round and node ID specified.
  """

  @spec init([args_t()]) :: {:ok, Mempool.t()}
  def init(args) do
    Process.set_label(__MODULE__)

    args =
      args
      |> Keyword.validate!([
        :node_id,
        transactions: [],
        consensus: [],
        round: 1
      ])

    node_id = args[:node_id]

    EventBroker.subscribe_me([
      Node.Event.node_filter(node_id),
      filter_for_mempool()
    ])

    EventBroker.subscribe_me([
      Node.Event.node_filter(node_id),
      filter_for_mempool_execution_events()
    ])

    for {id, tx_w_backend} <- args[:transactions] do
      tx(args[:node_id], tx_w_backend, id)
    end

    consensus = args[:consensus]
    round = args[:round]

    for list <- consensus do
      execute(node_id, list)
    end

    {:ok, %__MODULE__{round: round, node_id: node_id}}
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @doc """
  I am a function to dump transactions.

  Given a node ID, I give all the transactions as currently stored in the
  corresponding Mempool state.
  """

  @spec tx_dump(String.t()) :: [Mempool.Tx.t()]
  def tx_dump(node_id) do
    GenServer.call(Registry.via(node_id, __MODULE__), :dump)
  end

  @doc """
  I am a launch function for a new transaction.

  Given a node ID with a {backend, tx} tuple, I launch a new transaction
  with a random ID, sending an appropriate event.

  Afterwards, the transaction code is sent to the Executor Engine to be
  assigned to a Worder, while the code wrapped in `Tx.t()` will be stored
  in Mempool's state.

  See `tx/3` for launching a transaction with a given ID.
  """

  @spec tx(String.t(), {Backends.backend(), Noun.t()}) :: :ok
  def tx(node_id, tx_w_backend) do
    tx(node_id, tx_w_backend, :crypto.strong_rand_bytes(16))
  end

  @doc """
  I am a launch function for a new transaction with a given ID.

  See `tx/2` for logic documentation. In constrast to it, I launch an new
  transaction with a particular given ID. This functionality is to be used
  only for replays and testing.
  """

  @spec tx(String.t(), {Backends.backend(), Noun.t()}, binary()) :: :ok
  def tx(node_id, tx_w_backend, id) do
    GenServer.cast(Registry.via(node_id, __MODULE__), {:tx, tx_w_backend, id})
  end

  @doc """
  I am the execution function.

  I receive a list of binaries, which I recognize as a partial order for
  block execution, sending an appropriate consensus submission event.

  Once launched, I send the list to the Executor.

  I am asynchronous, meaning that I do not block and blocks can be
  submitted before the last one got executed.

  If execution is susccesful, the Mempool will handle an appropriate
  message from the Executor, which will trigger block-creation.
  """

  @spec execute(String.t(), list(binary())) :: :ok
  def execute(node_id, ordered_list_of_txs) do
    GenServer.cast(
      Registry.via(node_id, __MODULE__),
      {:execute, ordered_list_of_txs}
    )
  end

  ############################################################
  #                      Public Filters                      #
  ############################################################

  @doc """
  I am a filter spec which filters for messages from the Backends module.
  """

  @spec worker_module_filter() :: EventBroker.Filters.SourceModule.t()
  def worker_module_filter() do
    %EventBroker.Filters.SourceModule{module: Anoma.Node.Transaction.Backends}
  end

  @doc """
  I am a filter spec which filters for Mempool-related messages.
  """

  @spec filter_for_mempool() :: Backends.ForMempoolFilter.t()
  def filter_for_mempool() do
    %Backends.ForMempoolFilter{}
  end

  def filter_for_mempool_execution_events() do
    %Backends.ForMempoolExecutionFilter{}
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  @impl true
  def handle_call(:dump, _from, state) do
    {:reply, state.transactions |> Map.keys(), state}
  end

  def handle_call(_, _, state) do
    {:reply, :ok, state}
  end

  @impl true
  def handle_cast({:tx, tx, tx_id}, state) do
    {:noreply, handle_tx(tx, tx_id, state)}
  end

  def handle_cast({:execute, id_list}, state) do
    handle_execute(id_list, state)
    {:noreply, state}
  end

  def handle_cast(_, state) do
    {:noreply, state}
  end

  @impl true
  def handle_info(
        e = %EventBroker.Event{body: %Node.Event{body: %Events.ResultEvent{}}},
        state
      ) do
    {:noreply, handle_result_event(e, state)}
  end

  def handle_info(
        e = %EventBroker.Event{
          body: %Node.Event{body: %Anoma.Node.Events.ExecutionEvent{}}
        },
        state
      ) do
    {:noreply, handle_execution_event(e, state)}
  end

  def handle_info(_, state) do
    {:noreply, state}
  end

  ############################################################
  #                 Genserver Implementation                 #
  ############################################################

  @spec handle_tx({Backends.backend(), Noun.t()}, binary(), t()) :: t()
  defp handle_tx(tx = {backend, code}, tx_id, state = %Mempool{}) do
    value = %Tx{backend: backend, code: code}
    node_id = state.node_id

    Events.transaction_event(tx, tx_id, node_id)

    Executor.launch(node_id, tx, tx_id)

    %Mempool{
      state
      | transactions: Map.put(state.transactions, tx_id, value)
    }
  end

  @spec handle_execute(list(binary()), t()) :: :ok
  defp handle_execute(id_list, state = %Mempool{}) do
    Events.consensus_event(id_list, state.node_id)
    Executor.execute(state.node_id, id_list)
  end

  @spec handle_result_event(EventBroker.Event.t(), t()) :: t()
  defp handle_result_event(e, state = %Mempool{}) do
    id = e.body.body.tx_id
    res = e.body.body.vm_result

    new_map =
      state.transactions
      |> Map.update!(id, fn tx ->
        Map.put(tx, :vm_result, res)
      end)

    %Mempool{state | transactions: new_map}
  end

  @spec handle_execution_event(EventBroker.Event.t(), t()) :: t()
  defp handle_execution_event(e, state = %Mempool{}) do
    execution_list = e.body.body.result
    round = state.round
    node_id = state.node_id

    {writes, map} = process_execution(state, execution_list)

    Storage.commit(node_id, round, writes)

    Events.block_event(Enum.map(execution_list, &elem(&1, 1)), round, node_id)

    %Mempool{state | transactions: map, round: round + 1}
  end

  ############################################################
  #                           Helpers                        #
  ############################################################

  @spec process_execution(t(), [{{:ok, any()} | :error, binary()}]) ::
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
