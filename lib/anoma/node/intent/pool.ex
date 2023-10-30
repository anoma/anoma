defmodule Anoma.Node.Intent.Pool do
  @moduledoc """

  """

  alias __MODULE__
  use TypedStruct
  use GenServer

  alias Anoma.Intent
  alias Anoma.Node.Utility

  @type intents :: MapSet.t(Intent.t())
  typedstruct do
    field(:intents, intents, default: MapSet.new())
  end

  def init(_init) do
    {:ok, %Pool{}}
  end

  def start_link(arg) do
    GenServer.start_link(__MODULE__, arg, Utility.name(arg))
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @spec intents(GenServer.server()) :: intents
  def intents(pool) do
    GenServer.call(pool, :intents)
  end

  @spec new_intent(GenServer.server(), Intent.t()) :: :ok
  def new_intent(pool, intent) do
    GenServer.cast(pool, {:new_intent, intent})
  end

  @spec remove_intent(GenServer.server(), Intent.t()) :: :ok
  def remove_intent(pool, intent) do
    GenServer.cast(pool, {:remove_intent, intent})
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_cast({:new_intent, intent}, pool) do
    new_pool = handle_new_intent(pool, intent)
    {:noreply, new_pool}
  end

  def handle_cast({:remove_intent, intent}, pool) do
    new_pool = handle_remove_intent(pool, intent)
    {:noreply, new_pool}
  end

  def handle_call(:intents, _from, pool) do
    {:reply, pool.intents, pool}
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  @spec handle_remove_intent(t(), Intent.t()) :: t()
  defp handle_remove_intent(pool, intent) do
    %Pool{pool | intents: MapSet.delete(pool.intents, intent)}
  end

  @spec handle_new_intent(t(), Intent.t()) :: t()
  defp handle_new_intent(pool, intent) do
    %Pool{pool | intents: MapSet.put(pool.intents, intent)}
  end
end
