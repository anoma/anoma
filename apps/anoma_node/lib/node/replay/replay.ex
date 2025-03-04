defmodule Anoma.Node.Replay do
  @moduledoc """
  I contain logic to replay storage.

  When a node starts, it can start with an empty database, a "fresh" node.
  If there was pre-existing data present, it's called an an "existing node".

  In case of a fresh node, the node can start up without any ceremony. The database
  is created and the node starts up.

  In case of an existing node, the exising data needs to be verified for correctness.
  The replay mechanism assures that the present data is correct.

  Correct data means that the data in the values and events table is correct and does not lead to
  errors.

  A replay uses the previous data to start up a temporary node with the existing data.
  If that node starts up succesfully, the data is considered valid.
  The real node can continue starting up using the old data.
  The mock node is removed from the system.
  """
  alias Anoma.Node.Transaction.Backends
  alias Anoma.Node.Tables
  alias Anoma.Node.Replay.State

  require Logger

  ############################################################
  #                       Types                              #
  ############################################################

  @type block_info :: {integer(), integer()}
  @type round :: integer()
  @type consensi :: [[binary()]]
  @type transactions :: [Backends.transaction()]
  @type replay_data :: {consensi, round, block_info, transactions}

  @type mempool_args :: [
          transactions: [any()],
          round: non_neg_integer(),
          consensus: [any()]
        ]
  @type ordering_args :: [next_height: non_neg_integer()]
  @type storage_args :: [uncommitted_height: non_neg_integer()]
  ############################################################
  #                       Public                             #
  ############################################################

  def wait_for_confirm(_temp_node_id, nil) do
    {:ok, :confirmed}
  end

  def wait_for_confirm(temp_node_id, _final_consensus) do
    result =
      receive do
        %{body: %{node_id: ^temp_node_id, body: %{result: _}}} ->
          {:ok, :confirmed}

        %{body: %{node_id: ^temp_node_id, body: %{task: _}}} ->
          {:error, :confirm_failed}
      after
        10000 ->
          {:error, :confirm_failed}
      end

    result
  end

  @doc """
  I attempt to replay the data for a given node id.
  """
  @spec replay_for(String.t()) ::
          {:ok, :replay_succeeded} | {:error, :replay_failed}
  def replay_for(node_id) do
    temp_node_id = temporary_node_id()

    with {:ok, _} <- init_tables_node(node_id, temp_node_id),
         {:ok, args} <- State.startup_arguments(node_id) do
      EventBroker.subscribe_me([])

      Anoma.Supervisor.start_node(
        node_id: temp_node_id,
        replay: false,
        transaction: args
      )

      final_consensus = List.last(args[:mempool][:consensus])

      case wait_for_confirm(temp_node_id, final_consensus) do
        {:ok, :confirmed} ->
          Anoma.Supervisor.stop_node(temp_node_id)
          EventBroker.unsubscribe_me([])
          {:ok, :replay_succeeded}

        _ ->
          Anoma.Supervisor.stop_node(temp_node_id)
          EventBroker.unsubscribe_me([])
          {:error, :replay_failed}
      end
    else
      {:error, :target_node_existed} ->
        {:error, :replay_failed}

      {:error, :failed_to_create_replay_node} ->
        {:error, :replay_failed}

      {:error, :startup_arguments_failed} ->
        {:error, :replay_failed}
    end
  end

  @spec init_tables_node(any(), binary()) ::
          {:error, :failed_to_create_replay_node | :target_node_existed}
          | {:ok, :data_initialized}
  @doc """
  Given a node id, I will create the tables for the node and initialize them with
  the data used to replay.
  I do not start a node, I only set up its tables.
  """
  def init_tables_node(from_node_id, to_node_id) do
    with {:ok, :created} <- Tables.initialize_tables_for_node(to_node_id),
         {:ok, :copied} <- duplicate_tables(from_node_id, to_node_id) do
      {:ok, :data_initialized}
    else
      {:ok, :existing} ->
        Logger.error("data found for replay node #{inspect(to_node_id)}")
        {:error, :target_node_existed}

      {:error, :failed_to_initialize_tables} ->
        Logger.error("failed to create replay node #{inspect(to_node_id)}")
        {:error, :failed_to_create_replay_node}
    end
  end

  @doc """
  I create a temporary node and return its node id.
  Ensure that the tables have been copied before creating the temporary node.
  """
  @spec temporary_node_id() :: String.t()
  def temporary_node_id() do
    Base.encode64("#{System.monotonic_time()}")
  end

  ############################################################
  #                       Private Helpers                    #
  ############################################################

  # @doc """
  # I copy the values and updates table from one node to another.
  # """
  @spec duplicate_tables(String.t(), String.t()) :: {:ok, :copied}
  defp duplicate_tables(from_node_id, to_node_id) do
    # copy the entire values table
    from_table = Tables.table_values(from_node_id)
    to_table = Tables.table_values(to_node_id)
    {:ok, :table_copied} = Tables.duplicate_table(from_table, to_table)

    # copy the entire updates table
    from_table = Tables.table_updates(from_node_id)
    to_table = Tables.table_updates(to_node_id)
    {:ok, :table_copied} = Tables.duplicate_table(from_table, to_table)

    {:ok, :copied}
  end
end
