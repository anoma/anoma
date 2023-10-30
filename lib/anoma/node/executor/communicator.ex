defmodule Anoma.Node.Executor.Communicator do
  @moduledoc """
  I Manage the Pub Sub behavior

  If new intents come in, I send it to all my subscribers.

  Further I have the job of communicating to the `Primary` node when I
  get information for it to process.

  The information that is communicated is:

  1. intent solutions
  2. new intents

  """
  alias __MODULE__
  use TypedStruct
  use GenServer
  alias Anoma.Intent

  typedstruct do
    field(:subscribers, list(pid()), default: [])
    field(:primary, atom())
  end

  # TODO fixup init
  def init(init: init_subscribers, name: name) do
    {:ok, %Communicator{subscribers: init_subscribers, primary: name}}
  end

  def init(init) do
    {:ok, %Communicator{subscribers: init}}
  end

  def start_link(arg) do
    # hack do better
    name = arg[:name]

    options =
      if name do
        [name: com_name(name)]
      else
        []
      end

    GenServer.start_link(__MODULE__, arg, options)
  end

  @spec com_name(atom()) :: atom()
  def com_name(name), do: (Atom.to_string(name) <> "_com") |> String.to_atom()

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  # partial transactions are intents

  @doc """
  Denotes to the system there is a new intent.

  This does a few things:

  1. broadcast the new intent to the primary
  2. broadcasts the new intent to all subscribers
  """
  @spec new_intent(pid(), Intent.t()) :: :ok
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
  @spec broadcast_intent(t(), Intent.t()) :: [any()]
  defp broadcast_intent(agent, intent) do
    # safe even on a null primary thanks to GenServer
    broadcast([agent.primary | agent.subscribers], intent)
  end

  # Dirty send, maybe consider what the structure of a subscriber is
  # further determine if it should live here
  @spec broadcast(list(), Intent.t()) :: [any()]
  defp broadcast(sub_list, intent) do
    sub_list
    # this is bad we are assuming GenServer, lets make this generic by
    # passing in a module to do a cast or call or something.
    |> Enum.map(fn sub -> GenServer.cast(sub, {:new_intent, intent}) end)
  end
end
