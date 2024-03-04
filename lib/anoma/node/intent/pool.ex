defmodule Anoma.Node.IntentPool do
  use GenServer

  @moduledoc """

  """

  alias __MODULE__
  use TypedStruct

  alias Anoma.Intent
  alias Anoma.Node.Router

  @type intents :: MapSet.t(Intent.t())
  typedstruct do
    field(:intents_topic, Router.addr())
    field(:intents, intents, default: MapSet.new())
  end

  def init(topic) do
    {:ok, %IntentPool{intents_topic: topic}}
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @spec intents(Router.addr()) :: intents
  def intents(pool) do
    Router.call(pool, :intents)
  end

  @spec new_intent(Router.addr(), Intent.t()) :: :ok
  def new_intent(pool, intent) do
    Router.cast(pool, {:new_intent, intent})
  end

  @spec remove_intent(Router.addr(), Intent.t()) :: :ok
  def remove_intent(pool, intent) do
    Router.cast(pool, {:remove_intent, intent})
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_cast({:new_intent, intent}, _, pool) do
    if MapSet.member?(pool.intents, intent) do
      {:noreply, pool}
    else
      Router.cast(pool.intents_topic, {:new_intent, intent})

      {:noreply,
       %IntentPool{pool | intents: MapSet.put(pool.intents, intent)}}
    end
  end

  def handle_cast({:remove_intent, intent}, _, pool) do
    if MapSet.member?(pool.intents, intent) do
      Router.cast(pool.intents_topic, {:remove_intent, intent})

      {:noreply,
       %IntentPool{pool | intents: MapSet.delete(pool.intents, intent)}}
    else
      {:noreply, pool}
    end
  end

  def handle_call(:intents, _, pool) do
    {:reply, pool.intents, pool}
  end
end
