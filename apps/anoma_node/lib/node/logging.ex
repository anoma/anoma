defmodule Anoma.Node.Logging do
  @moduledoc """
  Replay manager with logger functionality
  """

  alias __MODULE__
  alias Anoma.Node.Transaction
  alias Transaction.{Mempool, Ordering, Storage}

  use GenServer
  use TypedStruct

  require Logger

  @type flag :: :info | :debug | :error

  typedstruct module: LoggingEvent do
    field(:flag, Logging.flag())
    field(:msg, binary())
  end

  typedstruct do
    field(:table, atom(), default: __MODULE__.Events)
  end

  def start_link(arg \\ []) do
    GenServer.start_link(__MODULE__, arg, name: Logging)
  end

  @spec init(any()) :: {:ok, Logging.t()}
  def init(arg) do
    keylist = Keyword.validate!(arg, table: __MODULE__.Events)
    table = keylist[:table]

    with {:atomic, :ok} <-
           :mnesia.create_table(table, attributes: [:type, :body]) do
      :mnesia.transaction(fn ->
        :mnesia.write({table, :round, -1})
      end)
    end

    EventBroker.subscribe_me([logging_filter()])
    {:ok, %__MODULE__{}}
  end

  def handle_info(
        %EventBroker.Event{
          body: %Logging.LoggingEvent{
            flag: flag,
            msg: msg
          }
        },
        state
      ) do
    log_fun({flag, msg})
    {:noreply, state}
  end

  def handle_info(
        %EventBroker.Event{
          body: %Mempool.TxEvent{
            id: id,
            tx: %Mempool.Tx{backend: backend, code: code}
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
          body: %Mempool.ConsensusEvent{
            order: list
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
          body: %Mempool.BlockEvent{
            order: id_list,
            round: round
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

  defp log_fun({:debug, msg}), do: Logger.debug(msg)

  defp log_fun({:info, msg}), do: Logger.info(msg)

  defp log_fun({:error, msg}), do: Logger.error(msg)

  def logging_filter() do
    %__MODULE__.LoggingFilter{}
  end

  defp match(flag, table) do
    case :mnesia.read({table, flag}) do
      [] -> []
      [{_, ^flag, current_pending}] -> current_pending
    end
  end

  def replay(
        event_table \\ Logging.Events,
        updates_table \\ Storage.Updates,
        values_table \\ Storage.Values,
        block_table \\ Storage.Blocks
      ) do
    Supervisor.start_child(
      Anoma.Node.Supervisor,
      {Logging, [table: event_table]}
    )

    {:atomic, args} =
      :mnesia.transaction(fn ->
        pending = match(:consensus, event_table)
        round = match(:round, event_table)
        blocks = :mnesia.match_object({block_table, :_, :_})

        {committed_round, height} =
          for {_, n, block} <- blocks,
              reduce: {-1, 0} do
            {_block_round, length} -> {n, length + length(block)}
          end

        ordering_info =
          {Ordering, [next_height: height + 1]}

        storage_info =
          {Storage,
           [
             uncommitted_height: height,
             updates_table: updates_table,
             values_table: values_table,
             blocks_table: block_table
           ]}

        mempool_info =
          if committed_round == round do
            {Mempool,
             [
               transactions: replay_tx_list(event_table),
               round: round + 1,
               consensus: pending
             ]}
          else
            {executed, remaining} =
              Enum.split(pending, committed_round - round + 1)

            for id <- Enum.concat(executed) do
              :mnesia.delete({event_table, id})
            end

            {Mempool,
             [
               transactions: replay_tx_list(event_table),
               round: committed_round + 1,
               consensus: remaining
             ]}
          end

        [ordering_info, storage_info, mempool_info]
      end)

    launch_children(args)
  end

  defp launch_children(list) do
    Enum.map(list, &Supervisor.start_child(Transaction.Supervisor, &1))
  end

  defp replay_tx_list(event_table) do
    list = :mnesia.all_keys(event_table)

    for id <- List.delete(list, :consensus),
        reduce: [] do
      lst ->
        [{^event_table, ^id, tx_w_backend}] =
          :mnesia.read(event_table, id)

        [{id, tx_w_backend} | lst]
    end
  end
end
