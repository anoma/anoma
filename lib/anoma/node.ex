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
          ordering: {Id.Extern.t() | nil, Ordering.t()},
          executor: {Id.Extern.t() | nil, Executor.t()},
          mempool: {Id.Extern.t() | nil, Mempool.t()},
          storage: {Id.Extern.t() | nil, Storage.t()},
          dumper: {Id.Extern.t() | nil, Dumper.t()},
          storage_data: {Storage.t(), atom(), atom()},
          snapshot_path: Noun.t()
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

  @spec start_link(configuration()) :: GenServer.on_start()
  def start_link(args) do
    # strawman pending proper lockfiles
    # also need to clean this up once we're done
    unix_path = Anoma.System.Directories.data("local.sock")

    testing = args[:testing] || false
    should_socket? = Mix.env() in [:dev, :prod] and not testing

    if should_socket? && File.exists?(unix_path) do
      File.rm(unix_path)

      IO.puts(
        "Node Already Running, Replacing #{unix_path} to send messages to this node"
      )
    end

    name = args[:name]
    rocks = args[:use_rocks]

    node_settings = {kind, settings} = args[:settings]

    {storage, block_storage, log_table} = storage_data(node_settings)
    storage_setup(storage, block_storage, log_table, rocks)

    case kind do
      :from_dump ->
        tables =
          settings[:qualified] ++
            settings[:order] ++
            settings[:block_storage] ++ settings[:logger_table]

        tables
        |> Enum.map(fn x ->
          fn -> :mnesia.write(x) end |> :mnesia.transaction()
        end)

      _ ->
        nil
    end

    with {:ok, pid} <-
           GenServer.start_link(__MODULE__, settings, name: name) do
      case kind do
        :new_storage ->
          snap = settings[:snapshot_path]

          node = state(pid)
          Storage.put_snapshot(node.storage, hd(snap))

        _ ->
          :ok
      end

      if should_socket? do
        # dump the initial state so our keys are persisted
        Anoma.Dump.dump("node_keys.dmp", name)
      end

      {:ok, pid}
    end
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

  @spec init(engine_configuration() | Anoma.Dump.dump()) :: any()
  def init(args) do
    {log_id, log_st} = args[:logger]
    {clock_id, _clock_st} = args[:clock]
    {config_id, config_st} = args[:configuration]
    {ord_id, ord_st} = args[:ordering]
    {mem_id, mem_st} = args[:mempool]
    {ping_id, ping_st} = args[:pinger]
    {ex_id, ex_st} = args[:executor]
    {storage_id, storage_st} = args[:storage]
    {dump_id, dump_st} = args[:dumper]

    {:ok, router, transport} =
      start_router(args[:router], args[:transport], args[:router_state])

    {:ok, storage_topic} = new_topic(router, args[:storage_topic])

    {:ok, storage} =
      start_engine(
        router,
        Storage,
        %Storage{
          storage_st
          | namespace: [router.id.encrypt],
            topic: storage_topic
        },
        id: storage_id
      )

    {:ok, clock} =
      start_engine(
        router,
        Clock,
        [start: System.monotonic_time(:millisecond)],
        id: clock_id
      )

    {:ok, logger_topic} = new_topic(router, args[:logger_topic])

    {:ok, logger} =
      start_engine(
        router,
        Logger,
        %Logger{log_st | clock: clock, topic: logger_topic},
        id: log_id
      )

    {:ok, configuration} =
      start_engine(
        router,
        Anoma.Node.Configuration,
        %Anoma.Node.Configuration{config_st | logger: logger},
        id: config_id
      )

    {:ok, ordering} =
      start_engine(
        router,
        Ordering,
        %Ordering{ord_st | logger: logger, storage: storage},
        id: ord_id
      )

    {:ok, executor_topic} = new_topic(router, args[:executor_topic])

    {:ok, executor} =
      start_engine(
        router,
        Executor,
        %Executor{
          ex_st
          | router: router,
            logger: logger,
            topic: executor_topic,
            ambiant_env: %Nock{
              ex_st.ambiant_env
              | logger: logger,
                ordering: ordering
            }
        },
        id: ex_id
      )

    {:ok, mempool_topic} = new_topic(router, args[:mempool_topic])

    {:ok, mempool} =
      start_engine(
        router,
        Mempool,
        %Mempool{
          mem_st
          | logger: logger,
            topic: mempool_topic,
            ordering: ordering,
            executor: executor
        },
        id: mem_id
      )

    {:ok, pinger} =
      start_engine(
        router,
        Pinger,
        %Pinger{
          ping_st
          | mempool: mempool,
            logger: logger
        },
        id: ping_id
      )

    {:ok, dumper} =
      start_engine(
        router,
        Dumper,
        %Dumper{
          dump_st
          | configuration: configuration,
            logger: logger
        },
        id: dump_id
      )

    Anoma.Node.Pinger.start(pinger)
    Dumper.start(dumper)
    Anoma.Node.Router.set_logger(router, logger)

    if Mix.env() in [:dev, :prod] do
      Anoma.Node.Transport.start_server(
        transport,
        {:unix, Anoma.System.Directories.data("local.sock")}
      )
    end

    {:ok,
     %Node{
       router: router,
       transport: transport,
       ordering: ordering,
       executor: executor,
       mempool: mempool,
       pinger: pinger,
       clock: clock,
       configuration: configuration,
       logger: logger,
       logger_topic: logger_topic,
       executor_topic: executor_topic,
       mempool_topic: mempool_topic,
       storage: storage,
       storage_topic: storage_topic,
       dumper: dumper
     }}
  end

  @doc """
  Given minimal arguments, I create appropriate setup for the
  `:settings` argument for Node initialization.
  """
  @spec start_min(min_engine_configuration()) :: engine_configuration()
  def start_min(args) do
    env = Map.merge(%Nock{}, Map.intersect(%Nock{}, args |> Enum.into(%{})))
    storage = args[:storage_data]
    block_storage = args[:block_storage]
    log_table = args[:logger_table]

    %{
      clock: {nil, %Clock{}},
      configuration:
        {nil, %Anoma.Node.Configuration{configuration: args[:configuration]}},
      logger: {nil, %Logger{table: log_table}},
      ordering: {nil, %Ordering{}},
      executor: {nil, %Executor{ambiant_env: env}},
      mempool: {nil, %Mempool{block_storage: args[:block_storage]}},
      pinger: {nil, %Pinger{time: args[:ping_time]}},
      storage: {nil, storage},
      dumper:
        {nil,
         %Dumper{
           count: args[:count]
         }},
      storage_data: {storage, block_storage, log_table},
      snapshot_path: args[:snapshot_path]
    }
  end

  @doc """
  I give the storage from a node.

  This is useful when we want to bring storage to a new node.
  """
  @spec raw_storage(t()) :: Storage.t()
  def raw_storage(node = %__MODULE__{storage: storage}) when storage != nil do
    storage = node.storage |> Engine.get_state()
    %Storage{storage | topic: nil}
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
