defmodule Anoma.Node.Logging do
  @moduledoc """
  I am the Logging Engine.

  I combine the classic logger with replay functionality. In particular,
  I store the most recent data coming outside of a Node so that if it fails
  I can re-do the actions fed to me in a linear fashion.

  ### Public API

  I provide the following public functionality:

  #### Replay

  - `restart_with_replay/1`
  - `replay_args/1`
  - `try_launch/2`
  - `replay_setup/2`
  - `replay_table_clone/3`

  #### Other

  - `table_name/1`
  - `init_table/2`
  - `log_event/3`
  """

  alias __MODULE__
  alias Anoma.Node
  alias Node.{Registry, Transaction}
  alias Transaction.{Mempool, Storage}

  use EventBroker.DefFilter
  use GenServer
  use TypedStruct

  require Node.Event
  require Logger

  ############################################################
  #                         State                            #
  ############################################################

  @typedoc """
  I am the loggging message type flag.

  I specify what logging levels are currently supported by the Logging
  Engine.
  """
  @type flag :: :info | :debug | :warning | :error

  @typep startup_options() ::
           {:node_id, String.t()} | {:table, atom()} | {:rocks, bool()}

  typedstruct module: LoggingEvent do
    @typedoc """
    I am the type of a logging event.

    I specify the format of any logging message sent.

    ### Fields

    - `:flag` - The level at which the event ought to be logged.
    - `:msg` - A logging message.
    """

    field(:flag, Logging.flag())
    field(:msg, binary())
  end

  typedstruct do
    @typedoc """
    I am the type of the Logging Engine.

    I store a Node ID with which I am associated alongside a table which
    stores all relevant events.

    ### Fields

    - `:node_id` - The ID of the Node to which a Logging Engine
                   instantiation is bound.
    - `:table` - The name of the table to which all replay events are
                 written.
                 Default: __MODULE__.Events
    """

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

  ############################################################
  #                    Genserver Helpers                     #
  ############################################################

  @doc """
  I am the start_link function of the Logging Engine.

  I register the Engine with the supplied Node ID provided by the arguments
  and check that the table keyword has been provided.
  """

  @spec start_link(list(startup_options())) :: term()
  def start_link(args) do
    args = Keyword.validate!(args, [:node_id, :table, :rocks])
    name = Registry.via(args[:node_id], __MODULE__)
    GenServer.start_link(__MODULE__, args, name: name)
  end

  @doc """
  I am the initialization function for the Logging Engine.

  From the specified arguments, I get the Node ID, the table name, as well
  as the boolean indicating whether the table should be backed by RocksDB.

  I then initialize the table with the given name and backing options,
  subscribe to logging messages and then launch the Engine with the given
  options.
  """

  @impl true
  def init(args) do
    Process.set_label(__MODULE__)

    args =
      Keyword.validate!(args, [
        :node_id,
        rocks: false,
        table: __MODULE__.Events
      ])

    table =
      String.to_atom("#{args[:table]}_#{:erlang.phash2(args[:node_id])}")

    init_table(table, args[:rocks])

    node_id = args[:node_id]

    EventBroker.subscribe_me([
      Node.Event.node_filter(node_id),
      logging_filter()
    ])

    {:ok, %__MODULE__{node_id: node_id, table: table}}
  end

  ############################################################
  #                      Public Filters                      #
  ############################################################

  @doc """
  I am the logging filter.

  I filter for any incoming messages the Logging Engine cares about.
  """

  @spec logging_filter() :: LoggingFilter.t()
  def logging_filter() do
    %__MODULE__.LoggingFilter{}
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  @impl true
  def handle_info(
        e = %EventBroker.Event{
          body: %Node.Event{
            body: %Logging.LoggingEvent{}
          }
        },
        state
      ) do
    {:noreply, handle_logging_event(e, state)}
  end

  def handle_info(
        e = %EventBroker.Event{
          body: %Node.Event{
            body: %Mempool.TxEvent{}
          }
        },
        state
      ) do
    {:noreply, handle_tx_event(e, state)}
  end

  def handle_info(
        e = %EventBroker.Event{
          body: %Node.Event{
            body: %Mempool.ConsensusEvent{}
          }
        },
        state
      ) do
    {:noreply, handle_consensus_event(e, state)}
  end

  def handle_info(
        e = %EventBroker.Event{
          body: %Node.Event{
            body: %Mempool.BlockEvent{}
          }
        },
        state
      ) do
    {:noreply, handle_block_event(e, state)}
  end

  ############################################################
  #                 Genserver Implementation                 #
  ############################################################

  @spec handle_logging_event(EventBroker.Event.t(), t()) :: t()
  defp handle_logging_event(
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
    state
  end

  @spec handle_tx_event(EventBroker.Event.t(), t()) :: t()
  defp handle_tx_event(
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
    state
  end

  @spec handle_consensus_event(EventBroker.Event.t(), t()) :: t()
  defp handle_consensus_event(
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
    state
  end

  @spec handle_block_event(EventBroker.Event.t(), t()) :: t()
  defp handle_block_event(
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
    state
  end

  ############################################################
  #                        Replay                            #
  ############################################################

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

  @doc """
  I am a function trying to launch a node.

  Given some replay arguments, I provide the core logic to be run by a
  separate task to test whether the original data has been corrupted or
  note.

  Namely, a launch a Node with given replay arguments and make sure that
  the final consensus gets provided.
  """

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

  @doc """
  I am the function getting the replay arguments for replay.

  Given a current height alongside mempool info, I set the ordering and
  storage info appropriately and put all transaction trifecta arguments in
  an order.
  """

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
  I am the replay setup function, the first step in the replay process.

  All tables here are the original ones from which we get info.

  This gives me the height, the round, as well as the replay Mempool struct.
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
  I am the function making a table copy for replay.

  Given original updates and values table used by the Storage, I copy their
  data to separate tables for a new node. Used for trying a replay on a
  mock node.
  """
  @spec replay_table_clone(atom(), atom(), String.t()) :: any()
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

  ############################################################
  #                           Helpers                        #
  ############################################################

  @spec init_table(atom(), bool()) :: {:atomic, :ok} | {:aborted, term()}
  defp init_table(table, rocks) do
    :mnesia.delete_table(table)
    rocks_opt = Anoma.Utility.rock_opts(rocks)

    :mnesia.create_table(table, rocks_opt ++ [attributes: [:type, :body]])

    :mnesia.clear_table(table)

    :mnesia.transaction(fn ->
      :mnesia.write({table, :round, -1})
    end)
  end

  defp log_fun({:debug, msg}), do: Logger.debug(msg)

  defp log_fun({:info, msg}), do: Logger.info(msg)

  defp log_fun({:warning, msg}), do: Logger.warning(msg)

  defp log_fun({:error, msg}), do: Logger.error(msg)

  @doc """
  I am the log event function.

  I provide an interface to "log" new messages in an easy format.

  Given a Node ID, a flag, and a message, I create a new event with
  appropriate flag and message.
  """

  @spec log_event(String.t(), flag(), binary()) :: :ok
  def log_event(node_id, flag, msg) do
    Node.Event.new_with_body(node_id, %__MODULE__.LoggingEvent{
      flag: flag,
      msg: msg
    })
    |> EventBroker.event()
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

  @doc """
  I am the name of the event table.

  Given a Node ID, I create an appropriately named Event table for it.
  """

  @spec table_name(String.t()) :: atom()
  def table_name(node_id) do
    String.to_atom("#{Logging.Events}_#{:erlang.phash2(node_id)}")
  end
end
