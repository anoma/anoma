defmodule Anoma.Node.Storage.Communicator do
  @moduledoc """

  I am the communicator for the Ordering Node, please read my Ordering
  node to know more about how my API works

  """

  use GenServer
  use TypedStruct
  alias __MODULE__
  alias Anoma.Node.Storage.Ordering
  alias Anoma.Node.Utility
  alias Anoma.Storage

  typedstruct do
    field(:primary, atom(), require: true)
    field(:subscribers, MapSet.t(GenServer.server()), default: MapSet.new())
  end

  def init(name: name) do
    {:ok, %Communicator{primary: name}}
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

  @spec state(GenServer.server()) :: Ordering.t()
  defdelegate state(ordering), to: Ordering

  @spec next_order(GenServer.server()) :: non_neg_integer()
  defdelegate next_order(ordering), to: Ordering

  @spec true_order(GenServer.server(), any()) :: non_neg_integer() | nil
  defdelegate true_order(ordering, id), to: Ordering

  @spec new_order(GenServer.server(), Ordering.ordered_transactions()) ::
          :error | {:ok, any()}
  defdelegate new_order(ordering, ordered), to: Ordering

  @spec get_storage(GenServer.server()) :: Storage.t()
  defdelegate get_storage(ordering), to: Ordering

  @spec reset(GenServer.server()) :: :ok
  defdelegate reset(ordering), to: Ordering

  @spec hard_reset(GenServer.server(), atom()) :: :ok
  defdelegate hard_reset(ordering, initial_snapshot), to: Ordering

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  # Please give things to the subscribers

  def handle_call(:state, _from, com) do
    {:reply, Ordering.state(com.primary), com}
  end

  def handle_call(:next_order, _from, com) do
    {:reply, Ordering.next_order(com.primary), com}
  end

  def handle_call({:true_order, id}, _from, com) do
    {:reply, Ordering.true_order(com.primary, id), com}
  end

  def handle_call({:new_order, trans}, _from, com) do
    {:reply, Ordering.new_order(com.primary, trans), com}
  end

  def handle_call(:storage, _from, state) do
    {:reply, Ordering.get_storage(state.primary()), state}
  end

  def handle_cast(:reset, state) do
    Ordering.reset(state.primary())
    {:noreply, state}
  end

  def handle_cast({:hard_reset, initial}, state) do
    Ordering.hard_reset(state.primary(), initial)
    {:noreply, state}
  end
end
