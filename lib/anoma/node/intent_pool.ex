defmodule Anoma.Node.IntentPool do
  use GenServer

  @moduledoc """

  """

  alias __MODULE__
  use TypedStruct

  alias Anoma.Intent
  alias Anoma.Node.{Router, Logger}

  @type intents :: MapSet.t(Intent.t())
  typedstruct do
    field(:intents_topic, Router.addr())
    field(:intents, intents, default: MapSet.new())
    field(:logger, Router.Addr.t(), enforce: false)
  end

  def init({topic, logger}) do
    {:ok, %IntentPool{intents_topic: topic, logger: logger}}
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
      log_info({:new_f, intent, pool.logger})
      {:noreply, pool}
    else
      Router.cast(pool.intents_topic, {:new_intent, intent})
      log_info({:new_t, intent, pool.logger})

      {:noreply,
       %IntentPool{pool | intents: MapSet.put(pool.intents, intent)}}
    end
  end

  def handle_cast({:remove_intent, intent}, _, pool) do
    if MapSet.member?(pool.intents, intent) do
      log_info({:remove_t, intent, pool.logger})
      Router.cast(pool.intents_topic, {:remove_intent, intent})

      {:noreply,
       %IntentPool{pool | intents: MapSet.delete(pool.intents, intent)}}
    else
      log_info({:remove_f, intent, pool.logger})
      {:noreply, pool}
    end
  end

  def handle_call(:intents, _, pool) do
    intents = pool.intents
    log_info({:all, intents, pool.logger})
    {:reply, intents, pool}
  end

  ############################################################
  #                     Logging Info                         #
  ############################################################

  defp log_info({:all, intents, logger}) do
    Logger.add(
      logger,
      :info,
      "Intents viewed in pool: #{inspect(intents)}"
    )
  end

  defp log_info({:remove_t, intent, logger}) do
    Logger.add(
      logger,
      :info,
      "Intent removed from pool: #{inspect(intent)}"
    )
  end

  defp log_info({:remove_f, intent, logger}) do
    Logger.add(
      logger,
      :info,
      "Request to remove intent,
      yet no matching intent in pool: #{inspect(intent)}"
    )
  end

  defp log_info({:new_t, intent, logger}) do
    Logger.add(
      logger,
      :info,
      "Intent added to pool: #{inspect(intent)}"
    )
  end

  defp log_info({:new_f, intent, logger}) do
    Logger.add(
      logger,
      :info,
      "Request to add new intent,
      yet a matching intent is already in the pool: #{inspect(intent)}"
    )
  end
end
