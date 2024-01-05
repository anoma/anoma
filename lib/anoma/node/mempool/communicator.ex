defmodule Anoma.Node.Mempool.Communicator do
  alias __MODULE__

  use TypedStruct
  use GenServer
  alias Anoma.Node.Mempool.Primary
  alias Anoma.Node.Utility

  # TODO add subscribers
  typedstruct do
    field(:primary, GenServer.server(), require: true)
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
  @spec tx(GenServer.server(), Noun.t()) :: Anoma.Transaction.t()

  defdelegate state(server), to: Primary
  defdelegate execute(server), to: Primary
  defdelegate soft_reset(server), to: Primary
  defdelegate hard_reset(server), to: Primary
  defdelegate tx(server, tx_code), to: Primary

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_call(:state, _from, state) do
    {:reply, {state, Primary.state(state.primary)}, state}
  end

  def handle_call({:tx, tx_code}, _from, state) do
    {:reply, Primary.tx(state.primary, tx_code), state}
  end

  def handle_call(:execute, _from, state) do
    {:reply, Primary.execute(state.primary), state}
  end

  def handle_cast(:soft_reset, state) do
    Primary.soft_reset(state.primary)
    {:noreply, state}
  end

  def handle_cast(:hard_reset, state) do
    Primary.hard_reset(state.primary)
    {:noreply, state}
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################
end
