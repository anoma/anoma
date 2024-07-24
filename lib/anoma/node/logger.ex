defmodule Anoma.Node.Logger do
  @moduledoc """
  I am the Logger Engine, an implementation of the Local Logging Engine.

  I have a storage field which accepts the storage used for the logging
  tied to a specific node, as well as the address of the clock used for
  timestamping.

  I store and get the logging info using the keyspace method.

  ### Public API

  I provide the following public functionality:

  #### Adding Logs

  - `add/3`

  #### Getting Logs

  - `get/1`
  - `get/2`
  """

  alias List
  alias Anoma.Node.Storage
  alias Anoma.Node.Router
  alias Anoma.Node.Clock
  alias Anoma.Crypto.Id

  use TypedStruct
  use Router.Engine

  require Logger

  typedstruct do
    @typedoc """
    I am the type of the Logging Engine.

    I have the minimal fields required to store timestamped messages in the
    specified storage.

    ### Fields

    - `:storage` - The Storage Engine address used for storing.
    - `:clock` - The Clock Engine address used for timestamping.
    - `:topic` - The topic address for broadcasting.
    """

    field(:storage, Router.Addr.t())
    field(:clock, Router.Addr.t())
    field(:topic, Router.Addr.t())
  end

  @doc """
  I am the Logger Engine initialization function.

  ### Pattern-Matching Variations

  - `init(Logger.t())` - I initialize the Engine with the given state.
  - `init(args)` - I expect a keylist with `:storage`, `:clock`, and
                   `:topic` keys and launch the Engine with the appropriate
                   state.
  """

  @spec init(Anoma.Node.Logger.t()) :: {:ok, Anoma.Node.Logger.t()}
  def init(%Anoma.Node.Logger{} = state) do
    {:ok, state}
  end

  @spec init(
          list(
            {:storage, Storage.t()}
            | {:clock, Router.addr()}
            | {:topic, Router.addr()}
          )
        ) ::
          {:ok, Anoma.Node.Logger.t()}
  def init(args) do
    {:ok,
     %Anoma.Node.Logger{
       storage: args[:storage],
       clock: args[:clock],
       topic: args[:topic]
     }}
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @doc """
  I am the Logger add function, the implementation of LocalLoggingAppend of
  the Local Logging Engine of the Anoma Specification.

  I receive three arguments: an address of the logger, an atom specifying
  the urgency of the logging message and the logging message itself.

  If the logger address is indeed an address, I put the info inside the
  specified storage using `Storage.put/3`.

  The information is stored using a keyspace method. The message is hence
  attached to a list of 4 elements:

  1. ID of the logger calling for storage
  2. ID of the engine whose info we log or a PID of the worker process
  3. Timestamp relative to the clock engine used
  4. The urgency atom

  I then use the atom to call the Elixir logger in order to inform the
  user of any specific major logging event such as, e.g. failing
  workers.
  """

  @spec add(Router.addr() | nil, atom(), String.t()) :: :ok | nil
  def add(logger, atom, msg) do
    if logger do
      Router.cast(logger, {:add, logger, atom, msg})
    else
      log_fun({atom, msg})
    end
  end

  @doc """
  I am the Logger get function of one argument.

  Given a non-nil logger address, I get the entire keyspace related to the
  storage attached to the aforementioned logger ID using
  `Storage.get_keyspace/2`

  I return a list of 2-tuples. Its left element will be a list whose
  head is the ID of the specified logger, followed by the engine ID (or
  worker PID), the timestamp, and an atom specifying urgency of the logging
  message. Its right element will be the message.
  """

  @spec get(Router.addr() | nil) ::
          list({list(Id.Extern.t() | integer() | atom()), String.t()})
          | nil
  def get(logger) do
    unless logger == nil do
      Router.call(logger, {:get_all, logger})
    end
  end

  @doc """
  I am the Logger get function of two arguments.

  Given a non-nil logger address and an engine address, I get the keyspace
  which conatins all the info stored by the logger about the supplied
  engine using `Storage.get_keyspace/2`

  I return a list of 2-tuples. Its left element will be a list whose
  head is the ID of the specified logger, followed by the engine ID (or
  worker PID), the timestamp, and an atom specifying urgency of the logging
  message. Its right element will be the message.
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
    do_add(logger, atom, msg, addr, state)
    {:noreply, state}
  end

  def handle_call({:get_all, logger}, _from, state) do
    {:reply, do_get(logger.id.encrypt, state.storage), state}
  end

  def handle_call({:get, logger, engine}, _from, state) do
    {:reply, do_get(logger.id.encrypt, state.storage, engine), state}
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  @spec do_add(
          Router.Addr.t(),
          atom(),
          String.t(),
          Router.Addr.t(),
          Anoma.Node.Logger.t()
        ) :: :ok
  defp do_add(logger, atom, msg, addr, state) do
    topic = state.topic

    id = Router.Addr.id(addr)

    addr = encrypt_or_server(id)

    Storage.put(
      state.storage,
      [logger.id.encrypt, addr, Clock.get_time(state.clock), atom],
      msg
    )

    if logger != nil and
         logger |> Router.Addr.pid() |> Process.alive?() do
      Router.cast(topic, {:logger_add, addr, msg})
    end

    log_fun({atom, msg})
  end

  @spec do_get(binary(), Router.Addr.t(), pid() | Router.Addr.t() | nil) ::
          :absent
          | list({any(), Storage.qualified_value()})
          | {:atomic, any()}
  defp do_get(id, storage, engine \\ nil) do
    keyspace =
      cond do
        engine == nil -> [id]
        true -> [id, encrypt_or_server(engine)]
      end

    Storage.get_keyspace(storage, keyspace)
  end

  ############################################################
  #                          Helpers                         #
  ############################################################

  @spec encrypt_or_server(Id.Extern.t()) :: binary()
  defp encrypt_or_server(id) do
    if id do
      id.encrypt
    end || id |> Router.Addr.server() |> Atom.to_string()
  end

  defp log_fun({:debug, msg}), do: Logger.debug(msg)

  defp log_fun({:info, msg}), do: Logger.info(msg)

  defp log_fun({:error, msg}), do: Logger.error(msg)
end
