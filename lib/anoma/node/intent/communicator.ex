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
  alias Anoma.Node.Logger

  typedstruct do
    field(:subscribers, ACom.t(), default: ACom.new())
    field(:pool, atom(), enforce: true)
    field(:logger, atom())
  end

  def init(name: name, init: subscribers, logger: logger) do
    {:ok,
     %Communicator{
       pool: name,
       subscribers: subscribers,
       logger: logger
     }}
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
    logger = agent.logger
    pool = agent.pool

    if :ok == Pool.new_intent(pool, intent) do
      broadcast_intent(agent, intent)
    end

    log_info({:submit, pool, logger})

    {:noreply, agent}
  end

  def handle_cast({:remove_intent, intent}, agent) do
    logger = agent.logger
    pool = agent.pool

    if :ok == Pool.remove_intent(pool, intent) do
      broadcast_remove(agent, intent)
    end

    log_info({:remove, pool, logger})

    {:noreply, agent}
  end

  def handle_cast({:on_sub, new_sub}, agent) do
    broadcast_intents(agent, new_sub)

    log_info({:sub, new_sub, agent.logger})

    {:noreply, agent}
  end

  def handle_call(:intents, _from, agent) do
    pool = agent.pool
    intents = Pool.intents(pool)
    log_info({:all, pool, agent.logger})

    {:reply, intents, agent}
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

    log_info({:cast_new, com.subscribers, com.logger})
  end

  @spec broadcast_remove(t(), Intent.t()) :: :ok
  defp broadcast_remove(com, intent) do
    Utility.broadcast(
      com.subscribers,
      {:remove_intent, intent}
    )

    log_info({:cast_rem, com.subscribers, com.logger})
  end

  @spec broadcast_intents(t(), GenServer.server()) :: :ok
  defp broadcast_intents(agent, new_sub) do
    pool = agent.pool
    intents = Pool.intents(pool)
    log_info({:cast_int, new_sub, agent.logger})

    Utility.broadcast([new_sub], {:intents, intents})
  end

  ############################################################
  #                     Logging Info                         #
  ############################################################

  defp log_info({:submit, pool, logger}) do
    Logger.add(logger, self(), "Intent submit request for: #{inspect(pool)}")
  end

  defp log_info({:remove, pool, logger}) do
    Logger.add(logger, self(), "Intent remove request for: #{inspect(pool)}")
  end

  defp log_info({:all, pool, logger}) do
    Logger.add(logger, self(), "Intents viewed for: #{inspect(pool)}")
  end

  defp log_info({:cast_new, subs, logger}) do
    Logger.add(logger, self(), "Broadcast new intent to: #{inspect(subs)}")
  end

  defp log_info({:cast_rem, subs, logger}) do
    Logger.add(
      logger,
      self(),
      "Broadcast removed intent to: #{inspect(subs)}"
    )
  end

  defp log_info({:sub, sub, logger}) do
    Logger.add(logger, self(), "New sub added: #{inspect(sub)}")
  end

  defp log_info({:cast_int, sub, logger}) do
    Logger.add(logger, self(), "Broadcast intents to: #{inspect(sub)}")
  end
end
