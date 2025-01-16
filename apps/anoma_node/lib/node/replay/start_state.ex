defmodule Anoma.Node.Replay.State do
  @moduledoc """
  I define logic that determines the startup state for the node.

  When a node starts and data is present in the database, specific arguments have to
  be passed to the supervision tree in order for the node to pick up where it left off.

  In particular, the following data must be restored.

   - The pending transactions waiting for an ordering.
   - Consensi in the mempool that have not been turned into a block.
   - The height of the ordering engine.
   - The committed height of the storage engine.

  """

  alias Anoma.Node.Tables

  require Logger

  ############################################################
  #                       Types                              #
  ############################################################

  @type storage_args :: [uncommitted_height: non_neg_integer()]

  @type ordering_args :: [next_height: non_neg_integer()]

  @type mempool_args :: [
          transactions: [any()],
          next_round: non_neg_integer(),
          consensus: [any()]
        ]

  @type startup_args :: [
          mempool: mempool_args,
          storage: storage_args,
          ordering: ordering_args
        ]

  @type block_table_summary :: %{
          last_round: non_neg_integer,
          transaction_count: non_neg_integer
        }

  @type events_table_summary :: %{
          transactions: [any()],
          next_round: non_neg_integer(),
          consensus: [[String.t()]]
        }

  ############################################################
  #                       Public                             #
  ############################################################
  @doc """
  Given a node id, I will determine the startup arguments for the node
  depending on the data found in the database.

  If no data exists, the empty list is returned. There is no initial state.
  """

  @spec startup_arguments_or_default(String.t()) :: {:ok, startup_args | nil}
  def startup_arguments_or_default(node_id) do
    case startup_arguments(node_id) do
      {:error, :startup_arguments_failed} ->
        {:ok, []}

      {:ok, arguments} ->
        {:ok, arguments}
    end
  end

  @spec startup_arguments(String.t()) ::
          {:ok, startup_args} | {:error, :startup_arguments_failed}
  def startup_arguments(node_id) do
    with {:ok, storage} <- storage_arguments(node_id),
         {:ok, ordering} <- ordering_arguments(node_id),
         {:ok, mempool} <- mempool_arguments(node_id) do
      {:ok,
       [
         mempool: mempool,
         ordering: ordering,
         storage: storage
       ]}
    else
      _ ->
        {:error, :startup_arguments_failed}
    end
  end

  @spec storage_arguments(String.t()) ::
          {:ok, storage_args} | {:error, :storage_args_failed}
  def storage_arguments(node_id) do
    # read the blocks table for this node
    blocks_table = Tables.table_blocks(node_id)

    case block_table_summary(blocks_table) do
      {:ok, blocks_summary} ->
        {:ok, [uncommitted_height: blocks_summary.last_round]}

      {:error, _e} ->
        {:error, :storage_args_failed}
    end
  end

  @spec ordering_arguments(binary()) ::
          {:ok, ordering_args} | {:error, :ordering_args_failed}
  def ordering_arguments(node_id) do
    # read the blocks table for this node
    blocks_table = Tables.table_blocks(node_id)

    case block_table_summary(blocks_table) do
      {:ok, blocks_summary} ->
        {:ok, [next_height: blocks_summary.transaction_count + 1]}

      {:error, :block_summary_failed} ->
        {:error, :ordering_args_failed}
    end
  end

  @spec mempool_arguments(String.t()) ::
          {:ok, mempool_args} | {:error, :storage_args_failed}
  def mempool_arguments(node_id) do
    # read the blocks table to figure out what the latest block was
    blocks_table = Tables.table_blocks(node_id)
    blocks_summary = block_table_summary(blocks_table)

    # read the blocks table for this node
    events_table = Tables.table_events(node_id)
    events_summary = events_table_summary(events_table)

    case {blocks_summary, events_summary} do
      {{:ok, blocks_summary}, {:ok, events_summary}} ->
        # if the last round of the blocks table is higher,
        # drop the consensi for those blocks, as they are outdated.
        events_summary =
          if blocks_summary.last_round >= events_summary.next_round do
            # how many blocks is the events table outdated
            lag = blocks_summary.last_round - events_summary.next_round + 1

            # consensi that are not actually in a block already
            stale_consensi = Enum.take(events_summary.consensus, lag)

            # transactions that are not in a block already
            stale_transaction_ids = Enum.concat(stale_consensi)

            events_summary
            |> Map.put(:next_round, blocks_summary.last_round + 1)
            |> Map.update!(:consensus, fn consensi ->
              Enum.drop(consensi, lag)
            end)
            |> Map.update!(
              :transactions,
              &Enum.reject(&1, fn {id, _} -> id in stale_transaction_ids end)
            )
          else
            events_summary
          end

        {:ok,
         [
           transactions: events_summary.transactions,
           round: events_summary.next_round,
           consensus: events_summary.consensus
         ]}

      {{:error, _e}, _} ->
        {:error, :storage_args_failed}

      {_, {:error, _e}} ->
        {:error, :storage_args_failed}
    end
  end

  @doc """
  Given a node id, I determine if there is existing data for this node.
  """
  @spec initialize_storage(String.t()) :: {:ok, :existing_node | :new_node}
  def initialize_storage(node_id) do
    # check if the node has existing tables, and initialize them if need be.
    case Tables.initialize_tables_for_node(node_id) do
      {:ok, :created} ->
        {:ok, :new_node}

      {:ok, :existing} ->
        {:ok, :existing_node}

      {:error, _e} ->
        {:error, :failed_to_initialize_storage}
    end
  end

  ############################################################
  #                       Helpers                             #
  ############################################################

  @doc """
  I return a summary of all the required information from the blocks table.
  """
  @spec block_table_summary(atom()) ::
          {:ok, block_table_summary} | {:error, :block_summary_failed}
  def block_table_summary(table) do
    :mnesia.transaction(fn ->
      default_summary = %{last_round: 0, transaction_count: 0}

      case :mnesia.match_object({table, :_, :_}) do
        # no blocks found, return default empty block
        [] ->
          default_summary

        blocks ->
          blocks
          |> Enum.reduce(default_summary, fn {_, round, txs}, summary ->
            summary
            |> Map.update!(:last_round, &max(round, &1))
            |> Map.update!(:transaction_count, &(&1 + Enum.count(txs)))
          end)
      end
    end)
    |> case do
      {:atomic, summary} ->
        {:ok, summary}

      _e ->
        {:error, :block_summary_failed}
    end
  end

  @spec events_table_summary(any()) ::
          {:error, :events_summary_failed} | {:ok, events_table_summary}
  def events_table_summary(table) do
    :mnesia.transaction(fn ->
      default_summary = %{transactions: [], next_round: 1, consensus: []}

      # fetch the list of transactions from the events table
      # use guard specs to get all values, except the consensus and round values
      # pattern to destructure every record against
      matchhead = {:"$1", :"$2", :"$3"}

      # guards to filter out objects we're not interested in
      # consensus = {:"=:=", :"$2", :consensus}
      # round = {:"=:=", :"$2", :round}
      # guards = [not: {:orelse, consensus, round}]

      # values of the result we want to get back
      result = [{{:"$2", :"$3"}}]

      case :mnesia.select(table, [{matchhead, [], [result]}]) do
        # no blocks found, return default empty block
        [] ->
          default_summary

        data ->
          # reduce all fields into the summary
          data
          |> Enum.reduce(default_summary, fn
            [round: round], summary ->
              Map.put(summary, :next_round, round)

            [consensus: transaction_ids], summary ->
              Map.put(summary, :consensus, transaction_ids)

            [{tx_id, tr}], summary ->
              Map.update!(summary, :transactions, &[{tx_id, tr} | &1])
          end)
      end
    end)
    |> case do
      {:atomic, summary} ->
        {:ok, summary}

      e ->
        Logger.error(inspect(e))
        {:error, :events_summary_failed}
    end
  end
end
