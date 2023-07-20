defmodule Anoma.Communicator do
  @moduledoc """
  I Manage the Pub Sub behavior

  If new intents come in, I send it to all my subscribers
  """
  alias __MODULE__
  use TypedStruct
  use GenServer

  typedstruct do
    field(:subscribers, list(), default: [])
  end

  def init(init_subscribers) do
    {:ok, %Communicator{subscribers: init_subscribers}}
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  # partial transactions are intents

  @spec new_intent(pid(), any()) :: :ok
  def new_intent(communicator, intent) do
    GenServer.cast(communicator, {:new_intent, intent})
  end

  @spec subscribe(pid(), pid()) :: :ok
  def subscribe(communicator, subscriber) do
    GenServer.cast(communicator, {:subscribe, subscriber})
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_cast({:new_intent, intent}, agent) do
    broadcast_intent(agent, intent)
    {:noreply, agent}
  end

  def handle_cast({:subscribe, pid}, agent) do
    {:noreply, %Communicator{agent | subscribers: [pid | agent.subscribers]}}
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  # make this more interesting later
  @spec broadcast_intent(t(), any()) :: [any()]
  defp broadcast_intent(agent, intent) do
    broadcast(agent.subscribers, intent)
  end

  # Dirty send, maybe consider what the structure of a subscriber is
  # further determine if it should live here
  @spec broadcast(list(), any()) :: [any()]
  defp broadcast(sub_list, intent) do
    sub_list
    # this is bad we are assuming GenServer, lets make this generic by
    # passing in a module to do a cast or call or something.
    |> Enum.map(fn sub -> GenServer.cast(sub, {:new_intent, intent}) end)
  end
end
