defmodule Anoma.Node.Mempool.Communicator do
  @moduledoc """

  """

  use GenServer
  use TypedStruct

  alias __MODULE__
  alias Anoma.Node.Utility
  alias Anoma.Node.Mempool.Pool
  alias Anoma.Transaction

  typedstruct do
    field(:subscribers, MapSet.t(GenServer.server()), default: MapSet.new())
    field(:pool, atom(), require: true)
  end

  def init(name: name, subs: subscribers) do
    {:ok, %Communicator{pool: name, subscribers: subscribers}}
  end

  def start_link(arg) do
    GenServer.start_link(Communicator, arg, Utility.name(arg, &Utility.com_name/1))
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @spec new_transactions(GenServer.server(), Pool.transactions()) :: :ok
  @spec eject(GenServer.server(), Pool.transactions()) :: :ok
  @spec take(GenServer.server(), pos_integer()) :: list()
  @spec peek(GenServer.server(), pos_integer()) :: list()

  @doc """
  Denotes to the system there is a new transaction.

  This does a few things:

  1. broadcast the new transactions to the pool
  2. broadcasts the new transactions to all subscribers
  """
  defdelegate new_transactions(communicator, transactions), to: Pool
  defdelegate eject(communicator, transactions), to: Pool
  defdelegate take(communicator, number), to: Pool
  defdelegate peek(communicator, number), to: Pool

  @spec subscribe(GenServer.server(), GenServer.server()) :: :ok
  def subscribe(communicator, subscriber) do
    GenServer.cast(communicator, {:subscribe, subscriber})
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_cast({:new_transactions, t}, com) do
    Pool.new_transactions(com.pool, t)
    broadcast_transaction(com.pool, t)
    {:noreply, com}
  end

  def handle_cast({:eject, transactions}, com) do
    Pool.eject(com.pool, transactions)
    {:noreply, com}
  end

  def handle_cast({:subscribe, new_sub}, com) do
    subscribers = MapSet.put(com.subscribers, new_sub)
    {:noreply, %Communicator{com | subscribers: subscribers}}
  end

  def handle_call({:take, n}, _from, com) do
    {:reply, Pool.take(com.pool, n), com}
  end

  def handle_call({:peek, n}, _from, com) do
    {:reply, Pool.peek(com.pool, n), com}
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  @spec broadcast_transaction(t(), Transaction.t()) :: :ok
  defp broadcast_transaction(com, transaction) do
    Utility.broadcast(com.subscribers, {:new_transaction, transaction})
  end
end
