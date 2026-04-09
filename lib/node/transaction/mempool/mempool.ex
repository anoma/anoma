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
  """

  alias __MODULE__
  alias Anoma.Node
  alias Anoma.Node.Logging
  alias Anoma.Node.Registry
  alias Anoma.Node.Transaction.Backends
  alias Anoma.Node.Transaction.Executor
  alias Anoma.Node.Transaction.Ordering
  alias Anoma.Node.Transaction.Mempool.Events

  require Logger
  require Node.Event

  use EventBroker.DefFilter
  use GenServer
  use TypedStruct

  import Noun

  ############################################################
  #                      Keyspaces                           #
  ############################################################

  @cairo_keyspace MapSet.new([["anoma", "cairo"], ["anoma", "blob"]])
  @transparent_keyspace MapSet.new([
                          ["anoma", "transparent"],
                          ["anoma", "blob"]
                        ])

  ############################################################
  #                       Types                              #
  ############################################################

  @typedoc """
  I am the type of the Nock VM result.
  """
  @type vm_result :: {:ok, Noun.t()} | :vm_error | :in_progress

  @typedoc """
  I am the type of the transaction result.
  """
  @type tx_result :: {:ok, Noun.t()} | :error | :in_progress

  @typedoc """
  Type of the arguments the mempool genserver expects
  """
  @type args_t ::
          [
            node_id: String.t(),
            transactions: [{binary, Noun.t()}],
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

  defimpl Jason.Encoder, for: Tx do
    defp encode_maybe_noun(noun) when is_atom(noun) do
      noun
    end

    defp encode_maybe_noun({:ok, noun}) do
      encode_maybe_noun(noun)
    end

    defp encode_maybe_noun(noun) do
      jammed = Noun.Jam.jam(noun)
      Base.encode64(jammed)
    end

    @spec encode(Tx.t(), Jason.Encode.opts()) :: iodata()
    def encode(tx = %Tx{}, opts) do
      vm_result = encode_maybe_noun(tx.vm_result)
      tx_result = encode_maybe_noun(tx.tx_result)
      code = encode_maybe_noun(tx.code)

      Jason.Encode.map(
        %{
          code: code,
          tx_result: tx_result,
          backend: nil,
          vm_result: vm_result
        },
        opts
      )
    end
  end

  defimpl Noun.Nounable, for: Tx do
    @impl true
    def to_noun(t) do
      tx_result =
        case t.tx_result do
          {:ok, noun} -> ["ok" | noun]
          res -> Noun.Nounable.to_noun(res)
        end

      vm_result =
        case t.vm_result do
          {:ok, noun} -> ["ok" | noun]
          res -> Noun.Nounable.to_noun(res)
        end

      backend =
        case t.backend do
          {:debug_read_term, _} -> "read"
          res -> Noun.Nounable.to_noun(res)
        end

      [tx_result, vm_result, backend | t.code]
    end
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

  @spec init([args_t()]) :: {:ok, t(), {:continue, any()}}
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

    state = %__MODULE__{round: args[:round], node_id: node_id}

    {:ok, state,
     {:continue, {:load_state, args[:transactions], args[:consensus]}}}
  end

  @impl true
  def handle_continue({:load_state, transactions, consensus}, state) do
    node_id = state.node_id

    for {id, {_backend, tx_candidate}} <- transactions do
      tx(node_id, tx_candidate, id)
    end

    for list <- consensus do
      execute(node_id, list)
    end

    {:noreply, state}
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @doc """
  I am a function to dump transactions.

  Given a node ID, I give all the transactions as currently stored in the
  corresponding Mempool state.
  """
  @spec tx_dump(String.t()) :: [binary()]
  def tx_dump(node_id) do
    GenServer.call(Registry.via(node_id, __MODULE__), :dump)
  end

  @doc """
  I am a launch function for a new transaction.

  Given a node ID with a transaction candidate, I launch a new transaction
  with a random ID, sending an appropriate event.

  After running an initial Nock evaluation, I look at the keys the
  transaction reads from and writes to. Depending on these, a backend gets
  assigned.

  Afterwards, the transaction code is sent to the Executor Engine to be
  assigned to a Worker, while the code wrapped in `Tx.t()` will be stored
  in Mempool's state.

  I return the ID assigned by the Mempool to the launched transaction.
  """

  @spec tx(String.t(), Noun.t()) :: binary()
  def tx(node_id, tx_w_backend) do
    GenServer.call(
      Registry.via(node_id, __MODULE__),
      {:tx, tx_w_backend, Base.encode64(:crypto.strong_rand_bytes(16))}
    )
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

  @spec tx(String.t(), {Backends.backend(), Noun.t()}, binary()) :: :ok
  defp tx(node_id, tx_w_backend, id) do
    GenServer.cast(
      Registry.via(node_id, __MODULE__),
      {:tx_async, tx_w_backend, id}
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

  @spec filter_for_mempool() :: Backends.Events.ForMempoolFilter.t()
  def filter_for_mempool() do
    %Backends.Events.ForMempoolFilter{}
  end

  @spec filter_for_mempool_execution_events() ::
          Backends.Events.ForMempoolExecutionFilter.t()
  def filter_for_mempool_execution_events() do
    %Backends.Events.ForMempoolExecutionFilter{}
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  @impl true
  def handle_call(:dump, _from, state) do
    {:reply, state.transactions |> Map.keys(), state}
  end

  @impl true
  def handle_call({:tx, tx, tx_id}, _from, state) do
    {:reply, tx_id, handle_tx(tx, tx_id, state)}
  end

  @spec handle_call(term(), GenServer.from(), t()) :: {:reply, :ok, t()}
  def handle_call(_, _, state) do
    {:reply, :ok, state}
  end

  @impl true
  def handle_cast({:tx_async, tx, tx_id}, state) do
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
        e = %EventBroker.Event{
          body: %Node.Event{body: %Backends.Events.ResultEvent{}}
        },
        state
      ) do
    {:noreply, handle_result_event(e, state)}
  end

  def handle_info(
        e = %EventBroker.Event{
          body: %Node.Event{body: %Executor.Events.ExecutionEvent{}}
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

  @spec handle_tx(Noun.t(), binary(), t()) :: t()
  defp handle_tx(tx, tx_id, state = %Mempool{}) do
    with {:ok, code} <- cue_when_atom(tx),
         {:ok, [[reads | writes] | function]} <-
           Nock.nock(code, [9, 2, 0 | 1], %Nock{}),
         {:ok, tx_function} <-
           Nock.nock(function, [10, [6, 1 | tx_id], 0 | 1], %Nock{}),
         {:ok, reads_list} <- Noun.Nounable.List.from_noun(reads),
         {:ok, writes_list} <- Noun.Nounable.List.from_noun(writes),
         read_list_of_keys <-
           reads_list
           |> Enum.map(fn n_key ->
             {:ok, key} = Noun.Nounable.List.from_noun(n_key)
             key
           end),
         write_list_of_keys <-
           writes_list
           |> Enum.map(fn n_key ->
             {:ok, key} = Noun.Nounable.List.from_noun(n_key)
             key
           end) do
      reservations = %{
        read: MapSet.new(read_list_of_keys),
        write: MapSet.new(write_list_of_keys)
      }

      Ordering.reserve(state.node_id, tx_id, reservations)

      handle_keyspace(
        code,
        {reads_list, writes_list},
        tx_function,
        tx_id,
        state
      )
    else
      _ ->
        Logging.log_event(
          state.node_id,
          :error,
          "Could not process keyspace evaluation. ID: #{inspect(tx_id)}"
        )

        state
    end
  end

  defp handle_keyspace(_code, {reads, writes}, tx_function, tx_id, state)
       when is_noun_zero(writes) do
    node_id = state.node_id

    Executor.launch(node_id, {:read_only, tx_function}, tx_id, reads)

    state
  end

  defp handle_keyspace(code, {reads, writes}, tx_function, tx_id, state) do
    flag =
      cond do
        keyspace_check(writes, @cairo_keyspace) ->
          :cairo_resource

        keyspace_check(writes, @transparent_keyspace) ->
          :transparent_resource

        true ->
          if Mix.env() in [:test, :dev] do
            :debug_term_storage
          end
      end

    unless flag do
      Logging.log_event(
        state.node_id,
        :error,
        "No keyspace match. Writes: #{inspect(writes)}"
      )

      state
    else
      value = %Tx{code: code, backend: flag}
      node_id = state.node_id

      tx_event(tx_id, value, node_id)

      Executor.launch(node_id, {flag, tx_function}, tx_id, reads)

      %Mempool{
        state
        | transactions: Map.put(state.transactions, tx_id, value)
      }
    end
  end

  @spec handle_execute(list(binary()), t()) :: :ok
  defp handle_execute(id_list, state = %Mempool{}) do
    consensus_event(id_list, state.node_id)
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

    {writes, map, set_of_ids} = process_execution(state, execution_list)

    Ordering.commit(node_id, round, writes, set_of_ids)

    block_event(Enum.map(execution_list, &elem(&1, 1)), round, node_id)

    %Mempool{state | transactions: map, round: round + 1}
  end

  ############################################################
  #                           Helpers                        #
  ############################################################

  @spec block_event(list(binary), non_neg_integer(), String.t()) :: :ok
  defp block_event(id_list, round, node_id) do
    block_event =
      Node.Event.new_with_body(node_id, %Events.BlockEvent{
        order: id_list,
        round: round
      })

    EventBroker.event(block_event)
  end

  @spec tx_event(binary(), Mempool.Tx.t(), String.t()) :: :ok
  defp tx_event(tx_id, value, node_id) do
    tx_event =
      Node.Event.new_with_body(node_id, %Events.TxEvent{
        id: tx_id,
        tx: value
      })

    EventBroker.event(tx_event)
  end

  @spec consensus_event(list(binary()), String.t()) :: :ok
  defp consensus_event(id_list, node_id) do
    consensus_event =
      Node.Event.new_with_body(node_id, %Events.ConsensusEvent{
        order: id_list
      })

    EventBroker.event(consensus_event)
  end

  @spec process_execution(t(), [{{:ok, any()} | :error, binary()}]) ::
          {[Mempool.Tx.t()], %{binary() => Mempool.Tx.t()}, MapSet.t()}
  defp process_execution(state, execution_list) do
    for {tx_res, id} <- execution_list,
        reduce: {[], state.transactions, MapSet.new()} do
      {lst, ex_state, set} ->
        {tx_struct, map} =
          Map.get_and_update!(ex_state, id, fn _ -> :pop end)

        {[Map.put(tx_struct, :tx_result, tx_res) | lst], map,
         MapSet.put(set, id)}
    end
  end

  @spec cue_when_atom(Noun.t()) :: :error | {:ok, Noun.t()}
  defp cue_when_atom(tx_code) when Noun.is_noun_atom(tx_code) do
    Noun.Jam.cue(tx_code)
  end

  defp cue_when_atom(tx_code) do
    {:ok, tx_code}
  end

  @spec keyspace_check([[binary()]], MapSet.t()) :: bool()
  defp keyspace_check(writes, space) do
    writes
    |> Enum.map(&Enum.take(&1, 2))
    |> MapSet.new()
    |> MapSet.subset?(space)
  end
end
