defmodule Anoma.Node do
  @moduledoc """
  I act as a registry for Anoma Nodes

  There are two ways to launch a Node. Either with minimal
  arguments or having the full specification for all the
  engines, including their external ID's.

  All inputs should come in format of a list of atom-value
  2-tuples, `{:settings, map()}` specifying the info on
  all relevant engines and storages possibly including their
  tables presented in a dumped format, `{:name, atom()}`
  specifying the name of the node, and `{:new_storage, bool()}`
  specifying whether the table names, if supplied, need to be
  set-up without any deletions.

  For more info on format check `start_min/1`

  ## Minimal Arguments

    - `name` - name for this process
    - `snapshot_path` : [`atom()` | 0]
      - A snapshot location for the service (used in the worker)
    - `storage` : `Anoma.Node.Storage.t()` - The Storage tables to use
    - `block_storage` - a location to store the blocks produced

  ### Created Tables
    - `storage.qualified`
    - `storage.order`
    - `block_storage`
  """

  use GenServer
  use TypedStruct

  alias Anoma.Node.Router.Engine

  alias Anoma.Node.{
    Router,
    Logger,
    Clock,
    Executor,
    Mempool,
    Pinger,
    Storage,
    Dumper
  }

  alias Anoma.Node.Ordering
  alias Anoma.Crypto.Id
  alias __MODULE__

  typedstruct do
    field(:router, Router.addr(), enforce: true)
    field(:transport, Router.addr(), enforce: true)
    field(:ordering, Router.addr())
    field(:executor, Router.addr())
    field(:executor_topic, Router.addr())
    field(:mempool, Router.addr())
    field(:mempool_topic, Router.addr())
    field(:pinger, Router.addr())
    field(:clock, Router.addr())
    field(:logger, Router.addr())
    field(:logger_topic, Router.addr())
    field(:storage, Router.addr())
    field(:configuration, Router.addr())
    field(:storage_topic, Router.addr())
    field(:dumper, Router.addr())
  end

  @typedoc """
  I am the configuration type for the `Anoma.Node`.

  Ι contain information necessary for tweaking the behavior of the
  `Anoma.Node` application. See my `Fields` section for more
  information on how my behavior changes the node.

  ### Fields

   - `:use_rocks` - determines if we wish to use rocksdb, or use mnesia
   in memory database

   - `:settings` - are the engine specific and anoma node configuration
     settings. See `node_settings/0` for more details
   - `:testing` is a flag that specifies if the node being launched is
     for testing purposes. Currently this just affects if we take up
     the standard socket over the current `Anoma` instance or not.
  """
  @type configuration() :: [
          name: atom(),
          use_rocks: boolean(),
          settings: node_settings(),
          testing: boolean() | nil
        ]

  @typedoc """
  I am the node settings

  I can be resumed from two states, either from new/fresh storage in
  which I just have a `engine_configuration/0` or from a previously
  dumped configuration. In which case I contain `t:Anoma.Dump.dump/0`.

  If the mode is in `:new_storage`, then Ι just contain `Engine`
  specific configuration settings. However if I'm a `:from_dump` then
  Ι have a superset of that information containing `mnesia` table
  configuration information as well.
  """
  @type node_settings() ::
          {:new_storage, engine_configuration()}
          | {:from_dump, Anoma.Dump.dump()}

  @type min_engine_configuration() :: [
          ping_time: :no_timer | non_neg_integer(),
          name: atom(),
          snapshot_path: Noun.t(),
          storage_data: Storage.t(),
          block_storage: atom(),
          configuration: Anoma.Configuration.configuration_map() | nil
        ]

  @type engine_configuration() :: %{
          clock: {Id.Extern.t() | nil, Clock.t()},
          configuration: {Id.Extern.t() | nil, Anoma.Node.Configuration.t()},
          pinger: {Id.Extern.t() | nil, Pinger.t()},
          logger: {Id.Extern.t() | nil, Logger.t()},
          mempool: {Id.Extern.t() | nil, Mempool.t()}
        }

  @doc """
  I assume I am fed a list with atom-value 2-tuples
  :name, :settings, :use_rocks. The value of :settings is a 2-tuple
  with the first component matching either :new_storage or :from_dump.
  If the former case, I need a :snapshot_path key in the settings.
  For the latter case, check Anoma.Dump for format descriptions of
  the settings.

  I ensure that the storages are deleted and created.
  Afterwards, if the storage is truly new, I put the snapshot
  in the ordering. Otherwise, I simply repopulate the tables
  from a supplied list by the settings.
  If :use_rocks has value true, I use rocksdb as storage backend.
  """

  def start_link(_args) do
    GenServer.start(__MODULE__, :anoma)
  end

  @doc """

  I work just like `start_link/1` except I give back the pid directly
  if the node is already started.

  This is preferable in tests, as we wish to have them be robust upon
  rerunning.

  """
  @spec start_link_or_find_instance(configuration()) :: GenServer.on_start()
  def start_link_or_find_instance(args) do
    case start_link(args) do
      {:error, {:already_started, pid}} ->
        {:ok, pid}

      other ->
        other
    end
  end

  # @spec init(engine_configuration() | Anoma.Dump.dump()) :: any()
  # def init(args) do
  #   {log_id, log_st} = args[:logger]
  #   {clock_id, _clock_st} = args[:clock]
  #   {config_id, config_st} = args[:configuration]
  #   {mem_id, _mem_st} = args[:mempool]
  #   {ping_id, ping_st} = args[:pinger]

  #   {:ok, router, transport} =
  #     start_router(args[:router], args[:transport], args[:router_state])

  #   {:ok, clock} =
  #     start_engine(
  #       router,
  #       Clock,
  #       [start: System.monotonic_time(:millisecond)],
  #       id: clock_id
  #     )

  #   {:ok, logger} =
  #     start_engine(
  #       router,
  #       Logger,
  #       %Logger{log_st | clock: clock},
  #       id: log_id
  #     )

  #   {:ok, configuration} =
  #     start_engine(
  #       router,
  #       Anoma.Node.Configuration,
  #       %Anoma.Node.Configuration{config_st | logger: logger},
  #       id: config_id
  #     )

  #   {:ok, mempool} =
  #     start_engine(
  #       router,
  #       Mempool,
  #       nil,
  #       id: mem_id
  #     )

  #   {:ok, pinger} =
  #     start_engine(
  #       router,
  #       Pinger,
  #       %Pinger{
  #         ping_st
  #         | mempool: mempool,
  #           logger: logger
  #       },
  #       id: ping_id
  #     )

  #   Anoma.Node.Pinger.start(pinger)
  #   Anoma.Node.Router.set_logger(router, logger)

  #   if Mix.env() in [:dev, :prod] do
  #     Anoma.Node.Transport.start_server(
  #       transport,
  #       {:unix, Anoma.System.Directories.data("local.sock")}
  #     )
  #   end

  #   {:ok,
  #    %Node{
  #      router: router,
  #      transport: transport,
  #      mempool: mempool,
  #      pinger: pinger,
  #      clock: clock,
  #      configuration: configuration,
  #      logger: logger
  #    }}
  # end

  @doc """
  Given minimal arguments, I create appropriate setup for the
  `:settings` argument for Node initialization.
  """
  @spec start_min(min_engine_configuration()) :: engine_configuration()
  def start_min(args) do
    log_table = args[:logger_table]

    %{
      clock: {nil, %Clock{}},
      configuration:
        {nil, %Anoma.Node.Configuration{configuration: args[:configuration]}},
      logger: {nil, %Logger{table: log_table}},
      mempool: {nil, %Mempool{}},
      pinger: {nil, %Pinger{time: args[:ping_time]}}
    }
  end

  def state(nodes) do
    GenServer.call(nodes, :state)
  end

  def handle_call(:state, _from, state) do
    {:reply, state, state}
  end

  @spec storage_data(node_settings()) :: {Storage.t(), atom(), atom()}
  defp storage_data(node_settings) do
    case node_settings do
      {_flag, settings} -> settings[:storage_data]
    end
  end

  defp storage_setup(storage, block_storage, logger, rocks) do
    Storage.do_ensure_new(storage, rocks)

    :mnesia.delete_table(logger)
    Anoma.Node.Logger.init_table(logger)

    :mnesia.delete_table(block_storage)
    Anoma.Block.create_table(block_storage, rocks)
  end

  defp start_router(router, transport, router_state) do
    if router == nil || transport == nil || router_state == nil do
      Router.start()
    else
      Router.start({router, transport, router_state})
    end
  end

  # We try to make the signature the same, with minimal changes
  @spec start_engine(Router.addr(), atom(), any(), [{:id, Id.Extern.t()}]) ::
          any()
  defp start_engine(router, module, state, options) do
    if options[:id] == nil do
      Router.start_engine(router, module, state)
    else
      Router.start_engine(router, module, state, id: options[:id])
    end
  end

  defp new_topic(router, id) do
    if id == nil do
      Router.new_topic(router)
    else
      Router.new_topic(router, %Id{external: id})
    end
  end
end
