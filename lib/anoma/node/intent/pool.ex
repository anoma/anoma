defmodule Anoma.Node.Intent.Pool do
  @moduledoc """

  """

  alias __MODULE__
  use TypedStruct
  use GenServer

  alias Anoma.Intent
  alias Anoma.Node.Utility
  alias Anoma.Node.Logger

  @type intents :: MapSet.t(Intent.t())
  typedstruct do
    field(:intents, intents, default: MapSet.new())
    field(:logger, atom())
  end

  def init(args) do
    {:ok, %Pool{logger: args[:logger]}}
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

  @spec new_intent(GenServer.server(), Intent.t()) :: Intent.t()
  def new_intent(pool, intent) do
    GenServer.call(pool, {:new_intent, intent})
  end

  @spec remove_intent(GenServer.server(), Intent.t()) :: Intent.t()
  def remove_intent(pool, intent) do
    GenServer.call(pool, {:remove_intent, intent})
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_call({:new_intent, intent}, _from, agent) do
    {state, new_pool} = handle_new_intent(agent, intent)
    log_info({:new, agent.logger})

    {:reply, state, new_pool}
  end

  def handle_call({:remove_intent, intent}, _from, agent) do
    {state, new_pool} = handle_remove_intent(agent, intent)
    log_info({:remove, agent.logger})
    {:reply, state, new_pool}
  end

  def handle_call(:intents, _from, agent) do
    intents = agent.intents
    log_info({:all, intents, agent.logger})
    {:reply, intents, agent}
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  @spec handle_remove_intent(t(), Intent.t()) ::
          {:ok, t()} | {:not_in, t()}
  defp handle_remove_intent(pool, intent) do
    logger = pool.logger

    if MapSet.member?(pool.intents, intent) do
      log_info({:remove_t, intent, logger})

      {:ok, %Pool{pool | intents: MapSet.delete(pool.intents, intent)}}
    else
      log_info({:remove_f, intent, logger})

      {:not_in, pool}
    end
  end

  @spec handle_new_intent(t(), Intent.t()) ::
          {:ok, t()} | {:already_in, t()}
  defp handle_new_intent(pool, intent) do
    logger = pool.logger

    if MapSet.member?(pool.intents, intent) do
      log_info({:new_f, intent, logger})

      {:already_in, pool}
    else
      log_info({:new_t, intent, logger})

      {:ok, %Pool{pool | intents: MapSet.put(pool.intents, intent)}}
    end
  end

  ############################################################
  #                     Logging Info                         #
  ############################################################

  defp log_info({:new, logger}) do
    Logger.add(logger, self(), :info, "Asked to add new intent")
  end

  defp log_info({:remove, logger}) do
    Logger.add(logger, self(), :info, "Asked to remove intent")
  end

  defp log_info({:all, intents, logger}) do
    Logger.add(
      logger,
      self(),
      :info,
      "Intents viewed in pool: #{inspect(intents)}"
    )
  end

  defp log_info({:remove_t, intent, logger}) do
    Logger.add(
      logger,
      self(),
      :info,
      "Intent removed from pool: #{inspect(intent)}"
    )
  end

  defp log_info({:remove_f, intent, logger}) do
    Logger.add(
      logger,
      self(),
      :info,
      "Request to remove intent,
      yet no matching intent in pool: #{inspect(intent)}"
    )
  end

  defp log_info({:new_t, intent, logger}) do
    Logger.add(
      logger,
      self(),
      :info,
      "Intent added to pool: #{inspect(intent)}"
    )
  end

  defp log_info({:new_f, intent, logger}) do
    Logger.add(
      logger,
      self(),
      :info,
      "Request to add new intent,
      yet a matching intent is already in the pool: #{inspect(intent)}"
    )
  end
end
