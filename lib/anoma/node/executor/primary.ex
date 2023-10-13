defmodule Anoma.Node.Executor.Primary do
  @moduledoc """
  I represent the main logic checking functonality of an `Anoma.Node.Executor`.

  I can be communicated by, by my public API, often this is done by my
  `Anoma.Node.Communicator`, however everything can communicate with
  me like my `Communicator`.
  """

  alias __MODULE__
  use TypedStruct
  use GenServer

  alias Anoma.Intent
  alias Anoma.Node.Utility

  typedstruct do
    field(:intents, list(Intent.t()), default: [])
  end

  def init(_init) do
    {:ok, %Primary{intents: []}}
  end

  def start_link(arg) do
    GenServer.start_link(__MODULE__, arg, Utility.name(arg))
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  # mainly for easier debugging
  @spec dump_state(GenServer.server()) :: t()
  def dump_state(primary) do
    GenServer.call(primary, :dump_state)
  end

  @spec new_intent(GenServer.server(), Intent.t()) :: :ok
  def new_intent(primary, intent) do
    GenServer.cast(primary, intent)
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_cast({:new_intent, intent}, agent) do
    updated_state = handle_new_intent(intent, agent)
    {:noreply, updated_state}
  end

  def handle_call(:dump_state, _pid, basic) do
    {:reply, basic, basic}
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  @spec handle_new_intent(Intent.t(), t()) :: t()
  defp handle_new_intent(intent, agent) do
    %Primary{agent | intents: [intent | agent.intents]}
  end
end
