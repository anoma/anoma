defmodule Anoma.Node.Logging do
  @moduledoc """
  Replay manager with logger functionality
  """

  alias __MODULE__
  alias Anoma.Node.Transaction.{Mempool, Ordering, Storage}

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
    {:ok, keylist} = Keyword.validate(arg, table: __MODULE__.Events)
    table = keylist[:table]

    :mnesia.create_table(table, attributes: [:type, :body])
    # mock initial consensus for better replay logic
    :mnesia.transaction(fn ->
      :mnesia.write({table, :consensus, {nil, []}})
    end)

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
            order: list,
            round: round
          }
        },
        state
      ) do
    :mnesia.transaction(fn ->
      :mnesia.write({state.table, :consensus, {round, list}})
    end)

    log_fun({:info, "Consensus provided order. Round: #{inspect(round)}"})
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

  def replay(event_table, updates_table, values_table, block_table) do
    :mnesia.transaction(fn ->
      # first read the latest consensus
      [{_, :consensus, {round, consensus_list}}] =
        :mnesia.read(event_table, :consensus)

      # get all blocks submitted
      blocks = :mnesia.match_object({block_table, :_, :_})

      # calculate the height by summing all block lengths
      height =
        for {_, _, block} <- blocks,
            reduce: 0 do
          count -> count + length(block)
        end

      # launch ordering and storage with suggested heights
      Ordering.start_link(next_height: height + 1)

      Storage.start_link(
        uncommitted_height: height,
        updates_table: updates_table,
        values_table: values_table,
        blocks_table: block_table
      )

      case :mnesia.read(block_table, round) do
        [{_, ^round, _}] ->
          # if the round has been already written, then either the consensus has
          # not been submitted for next round or the block write event was not
          # delivered so check that all consensus ids are deleted and launch the
          # rest of on Mempool initialization

          for id <- consensus_list do
            :mnesia.delete({event_table, id})
          end

          tx_list = replay_tx_list(event_table)

          Mempool.start_link(txs: tx_list, round: round + 1)

        [] ->
          # if no round has been written, then the consensus has not been
          # executed, hence we replay all events with the given consensus

          tx_list = replay_tx_list(event_table)

          Mempool.start_link(
            txs: tx_list,
            consensus: consensus_list,
            round: round
          )
      end
    end)
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
