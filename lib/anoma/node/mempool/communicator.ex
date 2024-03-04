defmodule Anoma.Node.Mempool.Communicator do
  alias __MODULE__

  use Anoma.Communicator, sub_field: :subscribers
  alias Anoma.Communicator, as: ACom

  use TypedStruct
  alias Anoma.Transaction
  alias Anoma.Node.Mempool.Primary
  alias Anoma.Node.{Utility, Logger}

  typedstruct do
    field(:primary, GenServer.server(), enforce: true)
    field(:subscribers, ACom.t(), default: ACom.new())
    field(:logger, atom(), enforce: false)
  end

  def init(args) do
    name = args[:name]

    {:ok,
     %Communicator{
       primary: name,
       logger: args[:logger]
     }}
  end

  def start_link(arg) do
    GenServer.start_link(
      __MODULE__,
      arg,
      Utility.name(arg, &Utility.com_name/1)
    )
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @spec state(GenServer.server()) :: Primary.t()
  @spec execute(GenServer.server()) :: non_neg_integer()
  @spec soft_reset(GenServer.server()) :: :ok
  @spec hard_reset(GenServer.server()) :: :ok
  @spec tx(GenServer.server(), Transaction.execution()) ::
          Anoma.Transaction.t()
  @spec pending_txs(GenServer.server()) :: list(Anoma.Transaction.t())

  defdelegate state(server), to: Primary
  defdelegate execute(server), to: Primary
  defdelegate soft_reset(server), to: Primary
  defdelegate hard_reset(server), to: Primary
  defdelegate tx(server, tx_code), to: Primary
  defdelegate pending_txs(server), to: Primary

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_call(:state, _from, state) do
    primary = state.primary
    log_info({:state, primary, state.logger})

    {:reply, {state, Primary.state(primary)}, state}
  end

  def handle_call({:tx, tx_code}, _from, state) do
    primary = state.primary
    tx = Primary.tx(primary, tx_code)
    log_info({:tx, primary, state.logger})

    {:reply, tx, state, {:continue, {:broadcast_tx, tx}}}
  end

  def handle_call(:execute, _from, state) do
    primary = state.primary
    ex = Primary.execute(primary)
    log_info({:execute, primary, state.logger})

    {:reply, ex, state, {:continue, {:broadcast_ex, ex}}}
  end

  def handle_call(:pending_txs, _from, state) do
    primary = state.primary
    log_info({:pending, primary, state.logger})

    {:reply, Primary.pending_txs(primary), state}
  end

  def handle_cast(:soft_reset, state) do
    primary = state.primary
    log_info({:soft, primary, state.logger})

    Primary.soft_reset(primary)
    {:noreply, state}
  end

  def handle_cast(:hard_reset, state) do
    primary = state.primary
    log_info({:hard, primary, state.logger})

    Primary.hard_reset(primary)
    {:noreply, state}
  end

  def handle_continue({:broadcast_tx, tx}, state) do
    subs = state.subscribers
    log_info({:broadcast_tx, state.primary, subs, state.logger})
    Utility.broadcast(subs, {:submitted, tx})
    {:noreply, state}
  end

  def handle_continue({:broadcast_ex, ex}, state) do
    subs = state.subscribers
    log_info({:broadcast_ex, state.primary, subs, state.logger})

    Utility.broadcast(subs, {:executed, ex})
    {:noreply, state}
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  ############################################################
  #                     Logging Info                         #
  ############################################################

  defp log_info({:state, primary, logger}) do
    Logger.add(
      logger,
      self(),
      :info,
      "Requested state from: #{inspect(primary)}"
    )
  end

  defp log_info({:tx, primary, logger}) do
    Logger.add(
      logger,
      self(),
      :info,
      "Requested to add transaction from: #{inspect(primary)}"
    )
  end

  defp log_info({:execute, primary, logger}) do
    Logger.add(
      logger,
      self(),
      :info,
      "Requested to execute block from: #{inspect(primary)}"
    )
  end

  defp log_info({:pending, primary, logger}) do
    Logger.add(
      logger,
      self(),
      :info,
      "Requested pending transactions from: #{inspect(primary)}"
    )
  end

  defp log_info({:soft, primary, logger}) do
    Logger.add(
      logger,
      self(),
      :debug,
      "Requested soft reset from: #{inspect(primary)}"
    )
  end

  defp log_info({:hard, primary, logger}) do
    Logger.add(
      logger,
      self(),
      :debug,
      "Requested hard reset from: #{inspect(primary)}"
    )
  end

  defp log_info({:broadcast_tx, primary, subs, logger}) do
    Logger.add(logger, self(), :info, "Broadcasting transaction.
      Primary: #{inspect(primary)}.
      Subscribers: #{inspect(subs)}")
  end

  defp log_info({:broadcast_ex, primary, subs, logger}) do
    Logger.add(logger, self(), :info, "Broadcasting execution.
      Primar: #{inspect(primary)}
      Subscribers: #{inspect(subs)}")
  end
end
