defmodule Anoma.Node.Logger do
  @moduledoc """
  I am a logging engine. I have a storage field which accepts the
  storage used for the logging tied to a specific node, as well as
  the name of the clock used for timestamping.

  I store and get the logging info using the keyspace method.

  ### Public API

  I provide the following public functionality:

  - `add/3`
  - `get/1`
  - `get/2`
  """

  alias List
  alias Anoma.Storage
  alias Anoma.Node.Router
  alias Anoma.Node.Clock
  alias Anoma.Crypto.Id

  use TypedStruct
  use Router.Engine

  require Logger

  # Maybe add timestamps from local wall clock as well?

  typedstruct do
    field(:storage, Storage.t())
    field(:clock, Router.Addr.t())
  end

  def init(%Anoma.Node.Logger{} = state) do
    {:ok, state}
  end

  @spec init(list({:storage, Storage.t()} | {:clock, Router.addr()})) ::
          {:ok, Anoma.Node.Logger.t()}
  def init(args) do
    {:ok, %Anoma.Node.Logger{storage: args[:storage], clock: args[:clock]}}
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @doc """
  I receive three arguments: an address of the logger, an atom specifying
  the urgency of the logging message and the logging message itself.

  If the logger address is indeed an address, I put the info inside the
  specified storage using `Storage.put/3`.

  The information is stored using a keyspace method. The message is hence
  attached to a list of 4 elements:

  1. ID of the logger calling for storage
  2. ID of the engine whose info we log
  3. Timestamp relative to the clock engine used
  4. The urgency atom

  I then use the atom to call the Elixir logger in order to inform the
  user of any specific major logging event such as, e.g. failing
  workers.
  """

  @spec add(Router.addr() | nil, atom(), String.t()) :: :ok | nil
  def add(logger, atom, msg) do
    unless logger == nil do
      Router.cast(logger, {:add, logger, atom, msg})
    end
  end

  @doc """
  Given a non-nil logger address, I get the entire keyspace related to the
  storage attached to the aforementioned logger ID using
  `Storage.get_keyspace/2`

  I return a list of 2-tuples. Its left element will be a list whose
  head is the ID of the specified logger, followed by the engine ID, the
  timestamp, and an atom specifying urgency of the logging message. Its
  right element will be the message.
  """
  @spec get(Router.addr() | nil) ::
          list({list(Id.Extern.t() | integer() | atom() | pid()), String.t()})
          | nil
  def get(logger) do
    unless logger == nil do
      Router.call(logger, {:get_all, logger})
    end
  end

  @doc """
  Given a non-nil logger address and an engine address, I get the keyspace
  which conatins all the info stored by the logger about the supplied
  engine using `Storage.get_keyspace/2`

  I return a list of 2-tuples. Its left element will be a list whose
  head is the ID of the specified logger, followed by the engine ID, the
  timestamp, and an atom specifying urgency of the logging message. Its
  right element will be the message.
  """

  @spec get(Router.addr() | nil, Router.addr() | pid()) ::
          list({list(Id.Extern.t() | integer() | atom() | pid()), String.t()})
          | nil
  def get(logger, engine) do
    unless logger == nil do
      Router.call(logger, {:get, logger, engine})
    end
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_cast({:add, logger, atom, msg}, addr, state) do
    id = addr.id

    address =
      if id == nil do
        addr.server
      else
        id
      end

    Storage.put(
      state.storage,
      [logger.id, address, Clock.get_time(state.clock), atom],
      msg
    )

    log_fun({atom, msg})

    {:noreply, state}
  end

  def handle_call({:get_all, logger}, _from, state) do
    {:reply, Storage.get_keyspace(state.storage, [logger.id]), state}
  end

  def handle_call({:get, logger, engine}, _from, state) do
    address =
      if is_pid(engine) do
        engine
      else
        engine.id
      end

    {:reply, Storage.get_keyspace(state.storage, [logger.id, address]), state}
  end

  ############################################################
  #                          Helpers                         #
  ############################################################

  defp log_fun({:debug, msg}), do: Logger.debug(msg)

  defp log_fun({:info, msg}), do: Logger.info(msg)

  defp log_fun({:error, msg}), do: Logger.error(msg)
end
