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
  alias Anoma.Node.Utility
  alias Anoma.Node.Time.Communicator

  require Logger

  # Maybe add timestamps from local wall clock as well?

  typedstruct do
    field(:storage, Storage.t())
    field(:clock, atom())
  end

  def init(args) do
    {:ok, %Anoma.Node.Logger{storage: args[:storage], clock: args[:clock]}}
  end

  def start_link(args) do
    GenServer.start_link(
      __MODULE__,
      args,
      Utility.name(args)
    )
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def add(logger, engine, atom, msg) do
    GenServer.cast(logger, {:add, logger, engine, atom, msg})
  end

  def get(logger) do
    GenServer.call(logger, {:get_all, logger})
  end

  def get(logger, engine) do
    GenServer.call(logger, {:get, logger, engine})
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  def handle_cast({:add, logger, engine, atom, msg}, state) do
    Storage.put(
      state.storage,
      [logger, engine, Communicator.get_time(state.clock), atom],
      msg
    )

    log_fun({atom, msg})

    {:noreply, state}
  end

  def handle_call({:get_all, logger}, _from, state) do
    {:reply, Storage.get_keyspace(state.storage, [logger]), state}
  end

  def handle_call({:get, logger, engine}, _from, state) do
    {:reply, Storage.get_keyspace(state.storage, [logger, engine]), state}
  end

  defp log_fun({:debug, msg}), do: Logger.debug(msg)

  defp log_fun({:info, msg}), do: Logger.info(msg)

  defp log_fun({:error, msg}), do: Logger.error(msg)
end
