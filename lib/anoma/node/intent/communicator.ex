defmodule Anoma.Node.Intent.Communicator do
  @moduledoc """


  """

  use Anoma.Communicator, sub_field: :subscribers, handle_sub: :on_sub
  alias Anoma.Communicator, as: ACom
  use TypedStruct
  alias __MODULE__
  alias Anoma.Intent
  alias Anoma.Node.Utility
  alias Anoma.Node.Intent.Pool

  typedstruct do
    field(:subscribers, ACom.t(), default: ACom.new())
    field(:pool, atom(), require: true)
  end

  def init(name: name, init: subscribers) do
    {:ok, %Communicator{pool: name, subscribers: subscribers}}
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

  @doc """
  Denotes to the system there is a new intent.

  This does a few things:

  1. broadcast the new intent to the pool
  2. broadcasts the new intent to all subscribers
  """
  @spec new_intent(GenServer.server(), Intent.t()) :: :ok
  def new_intent(communicator, intent) do
    GenServer.cast(communicator, {:new_intent, intent})
  end

  # TODO In reality we should accept evidence that it went through or
  # revoked

  @doc """
  Denotes to the system that an intent will be removed from the pool

  This does a few things:

  1. broadcast the new intent to the pool
  2. broadcasts the new intent to all subscribers
  """
  @spec remove_intent(GenServer.server(), Intent.t()) :: :ok
  def remove_intent(communicator, intent) do
    GenServer.cast(communicator, {:remove_intent, intent})
  end

  @spec all_intents(GenServer.server()) :: MapSet.t(Intent.t())
  def all_intents(communicator) do
    GenServer.call(communicator, :intents)
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_cast({:new_intent, intent}, agent) do
    if :ok == Pool.new_intent(agent.pool, intent) do
      broadcast_intent(agent, intent)
    end

    {:noreply, agent}
  end

  def handle_cast({:remove_intent, intent}, communicator) do
    if :ok == Pool.remove_intent(communicator.pool, intent) do
      broadcast_remove(communicator, intent)
    end

    {:noreply, communicator}
  end

  def handle_cast({:on_sub, new_sub}, agent) do
    broadcast_intents(agent, new_sub)
    {:noreply, agent}
  end

  def handle_call(:intents, _from, com) do
    {:reply, Pool.intents(com.pool), com}
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  @spec broadcast_intent(t(), Intent.t()) :: :ok
  defp broadcast_intent(com, intent) do
    Utility.broadcast(
      com.subscribers,
      {:new_intent, intent}
    )
  end

  @spec broadcast_remove(t(), Intent.t()) :: :ok
  defp broadcast_remove(com, intent) do
    Utility.broadcast(
      com.subscribers,
      {:remove_intent, intent}
    )
  end

  @spec broadcast_intents(t(), GenServer.server()) :: :ok
  defp broadcast_intents(agent, new_sub) do
    intents = Pool.intents(agent.pool)
    Utility.broadcast([new_sub], {:intents, intents})
  end
end
