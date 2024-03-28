defmodule Anoma.Node.Logger do
  @moduledoc """
  I am a logging engine. I have a storage field which accepts the
  storage used for the logging tied to a specific node, the number
  of messages which get stored per engine, as well as the name of
  the clock used for timestamping.
  """

  use TypedStruct
  use GenServer

  alias List
  alias Anoma.Storage
  alias Anoma.Node.Router
  alias Anoma.Node.Clock

  require Logger

  # Maybe add timestamps from local wall clock as well?

  typedstruct do
    field(:storage, Storage.t())
    field(:clock, Router.Addr.t())
    field(:router, Router.Addr.t())
  end

  def init(args) do
    {:ok,
     %Anoma.Node.Logger{
       storage: args[:storage],
       clock: args[:clock],
       router: args[:router]
     }}
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def add(logger, atom, msg) do
    unless logger == nil do
      Router.cast(logger, {:add, logger, atom, msg})
    end
  end

  def get(logger) do
    unless logger == nil do
      Router.call(logger, {:get_all, logger})
    end
  end

  def get(logger, engine) do
    unless logger == nil do
      Router.call(logger, {:get, logger, engine})
    end
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  def handle_cast({:add, logger, atom, msg}, addr, state) do
    Router.call(
      state.router,
      {:storage_put, [logger, addr.id, Clock.get_time(state.clock), atom],
       msg}
    )

    log_fun({atom, msg})

    {:noreply, state}
  end

  def handle_call({:get_all, logger}, _from, state) do
    {:reply, Router.call(state.router, {:storage_get_keyspace, [logger]}),
     state}
  end

  def handle_call({:get, logger, engine}, _from, state) do
    {:reply,
     Router.call(state.router, {:storage_get_keyspace, [logger, engine]}),
     state}
  end

  defp log_fun({:debug, msg}), do: Logger.debug(msg)

  defp log_fun({:info, msg}), do: Logger.info(msg)

  defp log_fun({:error, msg}), do: Logger.error(msg)
end
