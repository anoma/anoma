defmodule Anoma.Node.Logging do
  @moduledoc """
  Replay manager with logger functionality
  """

  alias __MODULE__
  alias Anoma.Node.Registry
  alias Anoma.Node.Transaction
  alias Transaction.Mempool

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
    %EventBroker.Event{body: %Anoma.Node.Logging.LoggingEvent{}} ->
      true

    %EventBroker.Event{body: %Mempool.TxEvent{}} ->
      true

    %EventBroker.Event{body: %Mempool.ConsensusEvent{}} ->
      true

    %EventBroker.Event{body: %Mempool.BlockEvent{}} ->
      true

    _ ->
      false
  end

  def start_link(args) do
    args = Keyword.validate!(args, [:node_id, :table, :rocks])
    name = Registry.via(args[:node_id], __MODULE__)
    GenServer.start_link(__MODULE__, args, name: name)
  end

  @spec init(any()) :: {:ok, Logging.t()}
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

    EventBroker.subscribe_me([logging_filter()])
    {:ok, %__MODULE__{node_id: args[:node_id], table: table}}
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
      pending = match_consensus(state.table)
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

      current_pending = match_consensus(state.table)
      :mnesia.write({state.table, :consensus, tl(current_pending)})
      :mnesia.write({state.table, :round, round})
    end)

    log_fun({:info, "Block succesfully committed. Round: #{inspect(round)}"})
    {:noreply, state}
  end

  @spec init_table(atom(), bool()) :: {:atomic, :ok}
  def init_table(table, rocks) do
    :mnesia.delete_table(table)
    rocks_opt = Anoma.Utility.rock_opts(rocks)

    {:atomic, :ok} =
      :mnesia.create_table(table, rocks_opt ++ [attributes: [:type, :body]])
  end

  defp log_fun({:debug, msg}), do: Logger.debug(msg)

  defp log_fun({:info, msg}), do: Logger.info(msg)

  defp log_fun({:error, msg}), do: Logger.error(msg)

  def logging_filter() do
    %__MODULE__.LoggingFilter{}
  end

  defp match_consensus(table) do
    case :mnesia.read({table, :consensus}) do
      [] -> []
      [{_, :consensus, current_pending}] -> current_pending
    end
  end

  def table_name(node_id) do
    String.to_atom("#{Logging.Events}_#{:erlang.phash2(node_id)}")
  end
end
