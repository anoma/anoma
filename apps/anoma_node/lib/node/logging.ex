defmodule Anoma.Node.Logging do
  @moduledoc """
  Replay manager with logger functionality
  """

  alias __MODULE__
  alias Anoma.Node
  alias Node.{Registry, Transaction}
  alias Transaction.{Mempool, Storage}

  use EventBroker.DefFilter
  use GenServer
  use TypedStruct

  require Logger

  @type flag :: :info | :debug | :error

  typedstruct module: LoggingEvent do
    field(:flag, Logging.flag())
    field(:msg, binary())
  end

  typedstruct do
    field(:node_id, String.t())
    field(:table, atom(), default: __MODULE__.Events)
  end

  deffilter LoggingFilter do
    %EventBroker.Event{
      body: %Node.Event{body: %Anoma.Node.Logging.LoggingEvent{}}
    } ->
      true

    %EventBroker.Event{body: %Node.Event{body: %Mempool.TxEvent{}}} ->
      true

    %EventBroker.Event{body: %Node.Event{body: %Mempool.ConsensusEvent{}}} ->
      true

    %EventBroker.Event{body: %Node.Event{body: %Mempool.BlockEvent{}}} ->
      true

    _ ->
      false
  end

  def start_link(args) do
    args = Keyword.validate!(args, [:node_id, :table])
    name = Registry.via(args[:node_id], __MODULE__)
    GenServer.start_link(__MODULE__, args, name: name)
  end

  @spec init(any()) :: {:ok, Logging.t()}
  def init(args) do
    Process.set_label(__MODULE__)

    args = Keyword.validate!(args, [:node_id, table: __MODULE__.Events])

    table =
      String.to_atom("#{args[:table]}_#{:erlang.phash2(args[:node_id])}")

    init_table(table)

    node_id = args[:node_id]

    EventBroker.subscribe_me([
      Node.Event.node_filter(node_id),
      logging_filter()
    ])

    {:ok, %__MODULE__{node_id: node_id, table: table}}
  end

  def handle_info(
        %EventBroker.Event{
          body: %Node.Event{
            body: %Logging.LoggingEvent{
              flag: flag,
              msg: msg
            }
          }
        },
        state
      ) do
    log_fun({flag, msg})
    {:noreply, state}
  end

  def handle_info(
        %EventBroker.Event{
          body: %Node.Event{
            body: %Mempool.TxEvent{
              id: id,
              tx: %Mempool.Tx{backend: backend, code: code}
            }
          }
        },
        state
      ) do
    :mnesia.transaction(fn ->
      :mnesia.write({state.table, id, {backend, code}})
    end)

    log_fun({:info, "Transaction Launched. Id: #{inspect(id)}"})
    {:noreply, state}
  end

  # when replaying, we need the round information to check against the comitted blocks
  def handle_info(
        %EventBroker.Event{
          body: %Node.Event{
            body: %Mempool.ConsensusEvent{
              order: list
            }
          }
        },
        state
      ) do
    :mnesia.transaction(fn ->
      pending = match(:consensus, state.table)
      :mnesia.write({state.table, :consensus, pending ++ [list]})
    end)

    log_fun({:info, "Consensus provided order. List: #{inspect(list)}"})
    {:noreply, state}
  end

  def handle_info(
        %EventBroker.Event{
          body: %Node.Event{
            body: %Mempool.BlockEvent{
              order: id_list,
              round: round
            }
          }
        },
        state
      ) do
    :mnesia.transaction(fn ->
      for id <- id_list do
        :mnesia.delete({state.table, id})
      end

      current_pending = match(:consensus, state.table)
      :mnesia.write({state.table, :consensus, tl(current_pending)})
      :mnesia.write({state.table, :round, round})
    end)

    log_fun({:info, "Block succesfully committed. Round: #{inspect(round)}"})
    {:noreply, state}
  end

  @spec init_table(atom()) :: {:atomic, :ok}
  def init_table(table) do
    :mnesia.delete_table(table)
    :mnesia.create_table(table, attributes: [:type, :body])

    :mnesia.clear_table(table)

    :mnesia.transaction(fn ->
      :mnesia.write({table, :round, -1})
    end)
  end

  defp log_fun({:debug, msg}), do: Logger.debug(msg)

  defp log_fun({:info, msg}), do: Logger.info(msg)

  defp log_fun({:error, msg}), do: Logger.error(msg)

  def logging_filter() do
    %__MODULE__.LoggingFilter{}
  end

  @doc """
  I am the function to be played on restarts of the Anoma node with known ID.

  I sync the event table with the blocks submitted and then launch a mock
  node with a new ID with replay arguments.

  If the replay succeeds, I reproduce it on the node with the ID provided.
  Otherwise, the only initialization information I reproduce are heights
  and rounds for the Transaction subsystem.
  """

  @spec restart_with_replay(String.t()) :: DynamicSupervisor.on_start_child()
  def restart_with_replay(node_id) do
    event_table = Logging.table_name(node_id)
    block_table = Storage.blocks_table(node_id)
    values_table = Storage.values_table(node_id)
    updates_table = Storage.updates_table(node_id)

    setup = replay_setup(event_table, block_table)
    mock_id = Node.prefix_random_id("mock")
    replay_table_clone(values_table, updates_table, mock_id)
    replay_args = replay_args(setup)

    res =
      try_launch(mock_id, replay_args)

    case res do
      :ok ->
        Anoma.Supervisor.start_node(node_id: node_id, tx_args: replay_args)

      :error ->
        base_args =
          replay_args
          |> Keyword.update!(
            :mempool,
            &Keyword.drop(&1, [:transactions, :consensus])
          )

        Anoma.Supervisor.start_node(node_id: node_id, tx_args: base_args)
    end
  end

  @spec try_launch(String.t(), any()) :: :ok | :error
  def try_launch(mock_id, replay_args) do
    try do
      EventBroker.subscribe_me([])

      {:ok, _pid} =
        Anoma.Supervisor.start_node(node_id: mock_id, tx_args: replay_args)

      final_consensus = List.last(replay_args[:mempool][:consensus])

      if final_consensus do
        receive do
          %EventBroker.Event{
            body: %Node.Event{
              node_id: ^mock_id,
              body: %Mempool.ConsensusEvent{
                order: ^final_consensus
              }
            }
          } ->
            :ok
        end
      end

      :ok
    rescue
      _e -> :error
    end
  end

  @spec replay_args(height: integer(), mempool: list()) :: [
          mempool: list(),
          ordering: list(),
          storage: list()
        ]
  def replay_args(height: height, mempool: mempool_info) do
    ordering_info =
      [next_height: height + 1]

    storage_info =
      [
        uncommitted_height: height
      ]

    [
      mempool: mempool_info,
      ordering: ordering_info,
      storage: storage_info
    ]
  end

  @doc """
  The first step in the replay process.

  All tables here are the original ones from which we get info.

  This gives us the height, the round, as well as the replay Mempool struct
  """

  @spec replay_setup(atom(), atom()) :: [height: integer(), mempool: list()]
  def replay_setup(event_table, block_table) do
    {:atomic, args} =
      :mnesia.transaction(fn ->
        pending = match(:consensus, event_table)
        round = match(:round, event_table)
        {committed_round, height} = block_match(block_table)

        mempool =
          process_mempool(committed_round, round, event_table, pending)

        [height: height, mempool: mempool]
      end)

    args
  end

  @doc """
  Copies the contents of the storage tables into mock Replay tables
  """
  def replay_table_clone(values_table, updates_table, node_id) do
    :mnesia.transaction(fn ->
      values = :mnesia.match_object({values_table, :_, :_})
      updates = :mnesia.match_object({updates_table, :_, :_})

      new_values_table =
        Storage.values_table(node_id)

      new_updates_table =
        Storage.updates_table(node_id)

      for {var, name} <- [
            {values, new_values_table},
            {updates, new_updates_table}
          ] do
        :mnesia.create_table(name, attributes: [:key, :value])

        for {_, key, value} <- var do
          :mnesia.write({name, key, value})
        end
      end
    end)
  end

  @spec process_mempool(integer(), integer(), atom(), list()) :: list()
  defp process_mempool(committed_round, round, event_table, pending) do
    if committed_round == round do
      [
        transactions: replay_tx_list(event_table),
        round: round + 1,
        consensus: pending
      ]
    else
      {executed, remaining} =
        Enum.split(pending, committed_round - round + 1)

      for id <- Enum.concat(executed) do
        :mnesia.delete({event_table, id})
      end

      [
        transactions: replay_tx_list(event_table),
        round: committed_round + 1,
        consensus: remaining
      ]
    end
  end

  @spec block_match(atom) :: {integer(), non_neg_integer()}
  defp block_match(block_table) do
    blocks =
      case :mnesia.match_object({block_table, :_, :_}) do
        [] -> [{:ok, -1, []}]
        res -> res
      end

    for {_, n, block} <- blocks,
        reduce: {-1, 0} do
      {_block_round, length} -> {n, length + length(block)}
    end
  end

  @spec replay_tx_list(atom()) :: list({binary(), any()})
  defp replay_tx_list(event_table) do
    list = :mnesia.all_keys(event_table)

    for id <- Enum.reject(list, fn x -> x == :consensus or x == :round end),
        reduce: [] do
      lst ->
        [{^event_table, ^id, tx_w_backend}] =
          :mnesia.read(event_table, id)

        [{id, tx_w_backend} | lst]
    end
  end

  @spec match(atom(), atom()) :: any()
  defp match(flag, table) do
    case :mnesia.read({table, flag}) do
      [] -> []
      [{_, ^flag, current_pending}] -> current_pending
    end
  end

  def table_name(node_id) do
    String.to_atom("#{Logging.Events}_#{:erlang.phash2(node_id)}")
  end
end
