defmodule Anoma.Node.Transaction.Narwhal.Worker do
  @moduledoc """
  I am the Narwhal Worker.

  I batch incoming transactions into groups, hash each batch to
  produce a digest, and store the batch data in mnesia for later
  retrieval by Bullshark.

  I subscribe to TxEvents from my own node and
  BatchDisseminateEvents from the validator network. When a batch
  is full (or a timer fires), I publish a BatchReadyEvent
  (node-local) and a BatchDisseminateEvent (cross-node).
  The timer always produces a batch each tick (even empty), which
  drives Narwhal round advancement autonomously.

  ### Public API

  - `get_batch/2`           - Look up tx_ids by digest (mnesia).
  - `get_full_batch/2`      - Look up tx_ids and tx_data (mnesia).
  - `has_batches?/2`        - Check batch availability (mnesia).
  - `store_fetched_batch/4` - Store a batch fetched from a peer.
  """

  alias Anoma.Node.Registry
  alias Anoma.Node.Transaction.Mempool
  alias Anoma.Node.Transaction.Narwhal.Events
  alias Anoma.Node.Transaction.Narwhal.Supervisor, as: NarwhalSup

  require Anoma.Node.Event

  use GenServer
  use TypedStruct

  @default_batch_size 10
  @flush_interval_ms 100

  ############################################################
  #                         State                            #
  ############################################################

  @type startup_options ::
          {:node_id, String.t()}
          | {:batch_size, pos_integer()}
          | {:config, Anoma.Node.Transaction.Narwhal.Config.t()}

  typedstruct do
    field(:node_id, String.t())
    field(:batch, [{binary(), term()}], default: [])
    field(:batch_size, pos_integer(), default: @default_batch_size)

    # Set after flushing a non-empty batch; cleared after
    # the next timer tick flushes an empty vote-round batch.
    field(:needs_vote_round, boolean(), default: false)
  end

  ############################################################
  #                    Public API                            #
  ############################################################

  @spec get_batch(String.t(), binary()) :: {:ok, [binary()]} | :absent
  def get_batch(node_id, digest), do: NarwhalSup.get_batch(node_id, digest)

  @spec get_full_batch(String.t(), binary()) ::
          {:ok, [binary()], %{binary() => term()}} | :absent
  def get_full_batch(node_id, digest),
    do: NarwhalSup.get_full_batch(node_id, digest)

  @spec has_batches?(String.t(), [binary()]) :: boolean()
  def has_batches?(node_id, digests),
    do: NarwhalSup.has_batches?(node_id, digests)

  @spec store_fetched_batch(String.t(), binary(), [binary()], %{
          binary() => term()
        }) ::
          :ok | :rejected
  def store_fetched_batch(node_id, digest, tx_ids, tx_data) do
    cond do
      NarwhalSup.get_batch(node_id, digest) != :absent ->
        :ok

      batch_digest(tx_ids, tx_data) != digest ->
        :rejected

      true ->
        register_foreign_tx_data(node_id, tx_ids, tx_data)
        NarwhalSup.store_batch(node_id, digest, tx_ids, tx_data)
        :ok
    end
  end

  ############################################################
  #                   GenServer Behavior                     #
  ############################################################

  @spec start_link(list(startup_options())) :: GenServer.on_start()
  def start_link(args) do
    node_id = Keyword.fetch!(args, :node_id)

    GenServer.start_link(__MODULE__, args,
      name: Registry.via(node_id, __MODULE__)
    )
  end

  @impl true
  def init(args) do
    Process.set_label(__MODULE__)

    node_id = Keyword.fetch!(args, :node_id)
    batch_size = Keyword.get(args, :batch_size, @default_batch_size)
    config = Keyword.fetch!(args, :config)

    EventBroker.subscribe_me([
      Anoma.Node.Event.node_filter(node_id),
      %Mempool.Events.TxFilter{}
    ])

    EventBroker.subscribe_me([
      %Events.ValidatorSetFilter{node_ids: config.node_id_set},
      %Events.BatchDisseminateFilter{}
    ])

    # Pre-store the empty batch digest so all validators can
    # validate blocks that reference empty batches without
    # needing cross-node dissemination.
    NarwhalSup.store_batch(node_id, batch_digest([], %{}), [], %{})

    Process.send_after(self(), :flush_timer, @flush_interval_ms)

    {:ok, %__MODULE__{node_id: node_id, batch_size: batch_size}}
  end

  @impl true
  def handle_info(
        %EventBroker.Event{
          body: %Anoma.Node.Event{
            body: %Mempool.Events.TxEvent{
              id: tx_id,
              tx: %Mempool.Tx{code: code}
            }
          }
        },
        state
      ) do
    new_batch = [{tx_id, code} | state.batch]
    state = %{state | batch: new_batch}

    if length(new_batch) >= state.batch_size do
      {:noreply, %{flush_batch(state) | needs_vote_round: true}}
    else
      {:noreply, state}
    end
  end

  @impl true
  def handle_info(
        %EventBroker.Event{
          body: %Anoma.Node.Event{
            body: %Events.BatchDisseminateEvent{from_node: own_id}
          }
        },
        state = %{node_id: own_id}
      ) do
    {:noreply, state}
  end

  @impl true
  def handle_info(
        %EventBroker.Event{
          body: %Anoma.Node.Event{
            body: %Events.BatchDisseminateEvent{
              digest: digest,
              tx_ids: tx_ids,
              tx_data: tx_data
            }
          }
        },
        state
      ) do
    if batch_digest(tx_ids, tx_data) == digest do
      register_foreign_tx_data(state.node_id, tx_ids, tx_data)
      NarwhalSup.store_batch(state.node_id, digest, tx_ids, tx_data)
      publish(state.node_id, %Events.ForeignBatchStoredEvent{digest: digest})
    end

    {:noreply, %{state | needs_vote_round: true}}
  end

  @impl true
  def handle_info(:flush_timer, state) do
    state =
      cond do
        state.batch != [] ->
          %{flush_batch(state) | needs_vote_round: true}

        state.needs_vote_round ->
          %{flush_empty_batch(state) | needs_vote_round: false}

        true ->
          state
      end

    Process.send_after(self(), :flush_timer, @flush_interval_ms)
    {:noreply, state}
  end

  @impl true
  def handle_info(_msg, state) do
    {:noreply, state}
  end

  ############################################################
  #                      Implementation                     #
  ############################################################

  defp publish(node_id, body) do
    EventBroker.event(Anoma.Node.Event.new_with_body(node_id, body))
  end

  @spec flush_batch(t()) :: t()
  defp flush_batch(state) do
    items = Enum.reverse(state.batch)
    tx_ids = Enum.map(items, &elem(&1, 0))
    tx_data = Map.new(items)
    digest = batch_digest(tx_ids, tx_data)

    if items != [] do
      NarwhalSup.store_batch(state.node_id, digest, tx_ids, tx_data)

      publish(state.node_id, %Events.BatchDisseminateEvent{
        digest: digest,
        tx_ids: tx_ids,
        tx_data: tx_data,
        from_node: state.node_id
      })
    end

    publish(state.node_id, %Events.BatchReadyEvent{digest: digest})

    %{state | batch: []}
  end

  @spec flush_empty_batch(t()) :: t()
  defp flush_empty_batch(state) do
    digest = batch_digest([], %{})
    publish(state.node_id, %Events.BatchReadyEvent{digest: digest})
    state
  end

  @spec register_foreign_tx_data(String.t(), [binary()], %{binary() => term()}) ::
          :ok
  defp register_foreign_tx_data(node_id, tx_ids, tx_data) do
    tx_pairs =
      Enum.flat_map(tx_ids, fn tx_id ->
        case Map.fetch(tx_data, tx_id) do
          {:ok, code} -> [{tx_id, code}]
          :error -> []
        end
      end)

    if tx_pairs != [], do: Mempool.register_foreign_txs(node_id, tx_pairs)

    :ok
  end

  @spec batch_digest([binary()], %{binary() => term()}) :: binary()
  defp batch_digest(tx_ids, tx_data) do
    committed =
      Enum.map(tx_ids, fn tx_id ->
        {tx_id,
         :crypto.hash(
           :sha256,
           :erlang.term_to_binary(Map.get(tx_data, tx_id))
         )}
      end)

    :crypto.hash(:sha256, :erlang.term_to_binary(committed))
  end
end
