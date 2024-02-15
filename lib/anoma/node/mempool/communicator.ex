defmodule Anoma.Node.Mempool.Communicator do
  alias __MODULE__

  use Anoma.Communicator, sub_field: :subscribers
  alias Anoma.Communicator, as: ACom

  use TypedStruct
  alias Anoma.Transaction
  alias Anoma.Node.Mempool.Primary
  alias Anoma.Node.Utility

  typedstruct do
    field(:primary, GenServer.server(), enforce: true)
    field(:subscribers, ACom.t(), default: ACom.new())
  end

  def init(args) do
    {:ok, %Communicator{primary: args[:name]}}
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
    {:reply, {state, Primary.state(state.primary)}, state}
  end

  def handle_call({:tx, tx_code}, _from, state) do
    tx = Primary.tx(state.primary, tx_code)
    {:reply, tx, state, {:continue, {:broadcast_tx, tx}}}
  end

  def handle_call(:execute, _from, state) do
    ex = Primary.execute(state.primary)
    {:reply, ex, state, {:continue, {:broadcast_ex, ex}}}
  end

  def handle_call(:pending_txs, _from, state) do
    {:reply, Primary.pending_txs(state.primary), state}
  end

  def handle_cast(:soft_reset, state) do
    Primary.soft_reset(state.primary)
    {:noreply, state}
  end

  def handle_cast(:hard_reset, state) do
    Primary.hard_reset(state.primary)
    {:noreply, state}
  end

  def handle_continue({:broadcast_tx, tx}, state) do
    Utility.broadcast(state.subscribers, {:submitted, tx})
    {:noreply, state}
  end

  def handle_continue({:broadcast_ex, ex}, state) do
    Utility.broadcast(state.subscribers, {:executed, ex})
    {:noreply, state}
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################
end
