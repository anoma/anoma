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
  alias Anoma.Node.{Router, Logger, Clock, Executor, Mempool, Pinger, Storage}
  alias Anoma.Node.Ordering
  alias Anoma.Crypto.Id
  alias __MODULE__

  typedstruct enforce: true do
    field(:router, Router.addr())
    field(:transport, Router.addr())
    field(:ordering, Router.addr())
    field(:executor, Router.addr())
    field(:executor_topic, Router.addr())
    field(:mempool, Router.addr())
    field(:mempool_topic, Router.addr())
    field(:pinger, Router.addr())
    field(:clock, Router.addr())
    field(:logger, Router.addr())
    field(:storage, Router.addr())
    field(:configuration, Router.addr())
    field(:storage_topic, Router.addr())
  end

  @type configuration() :: [
          new_storage: boolean(),
          name: atom(),
          use_rocks: boolean(),
          settings: engine_configuration(),
          configuration: Anoma.Configuration.configuration_map() | nil
        ]

  @type min_engine_configuration() :: [
          ping_time: :no_timer | non_neg_integer(),
          name: atom(),
          snapshot_path: Noun.t(),
          storage_data: Storage.t(),
          block_storage: atom(),
          configuration: Anoma.Configuration.configuration_map() | nil
        ]

  @type engine_configuration() :: %{
          clock: {Router.addr() | nil, Clock.t()},
          configuration: {Router.addr() | nil, Anoma.Node.Configuration.t()},
          pinger: {Router.addr() | nil, Pinger.t()},
          logger: {Router.addr() | nil, Logger.t()},
          ordering: {Router.addr() | nil, Ordering.t()},
          executor: {Router.addr() | nil, Executor.t()},
          mempool: {Router.addr() | nil, Mempool.t()},
          storage: {Router.addr() | nil, Storage.t()},
          storage_data: {Storage.t(), atom()},
          snapshot_path: Noun.t()
        }

  @doc """
  I assume I am fed a list with atom-value 2-tuples
  :new_storage, :name, :settings. If :new_storage has
  value true, I need a snapshot_path: key in the
  settings.

  I ensure that the storages are deleted and created.
  Afterwards, if the storage is truly new, I put the snapshot
  in the ordering. Otherwise, I simply repopulate the tables
  from a supplied list by the settings.
  Check Anoma.Dump for format descriptions.
  """

  @spec start_link(configuration()) :: GenServer.on_start()
  def start_link(args) do
    # strawman pending proper lockfiles
    # also need to clean this up once we're done
    unix_path = Anoma.System.Directories.data("local.sock")

    if Mix.env() in [:dev, :prod] && File.exists?(unix_path) do
      File.rm(unix_path)

      IO.puts(
        "Node Already Running, Replacing #{unix_path} to send messages to this node"
      )
    end

    settings = args[:settings]
    name = args[:name]
    rocks = args[:use_rocks]
    {storage, block_storage} = settings[:storage_data]
    storage_setup(storage, block_storage, rocks)

    unless args[:new_storage] do
      tables =
        settings[:qualified] ++ settings[:order] ++ settings[:block_storage]

      tables
      |> Enum.map(fn x ->
        fn -> :mnesia.write(x) end |> :mnesia.transaction()
      end)
    end

    with {:ok, pid} <- GenServer.start_link(__MODULE__, settings, name: name) do
      if args[:new_storage] do
        snap = settings[:snapshot_path]

        node = state(pid)
        Storage.put_snapshot(node.storage, hd(snap))
      end

      if Mix.env() in [:dev, :prod] do
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

  @spec init(Anoma.Dump.dump()) :: any()
  def init(args) do
    {log_id, log_st} = args[:logger]
    {clock_id, _clock_st} = args[:clock]
    {config_id, config_st} = args[:configuration]
    {ord_id, ord_st} = args[:ordering]
    {mem_id, mem_st} = args[:mempool]
    {ping_id, ping_st} = args[:pinger]
    {ex_id, ex_st} = args[:executor]
    {storage_id, storage_st} = args[:storage]

    {:ok, router, transport} =
      start_router(args[:router], args[:transport], args[:router_state])

    {:ok, storage_topic} = new_topic(router, args[:storage_topic])

    {:ok, storage} =
      start_engine(
        router,
        Storage,
        %Storage{storage_st | namespace: [router.id], topic: storage_topic},
        id: storage_id
      )

    {:ok, clock} =
      start_engine(
        router,
        Clock,
        [start: System.monotonic_time(:millisecond)],
        id: clock_id
      )

    {:ok, logger} =
      start_engine(
        router,
        Logger,
        %Logger{log_st | clock: clock, storage: storage},
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
        %Ordering{ord_st | logger: logger, table: storage},
        id: ord_id
      )

    {:ok, executor_topic} = new_topic(router, args[:executor_topic])

    {:ok, executor} =
      start_engine(
        router,
        Executor,
        %Executor{
          ex_st
          | logger: logger,
            task_completion_topic: executor_topic,
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
          | mempool: mempool
        },
        id: ping_id
      )

    Anoma.Node.Pinger.start(pinger)

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
       executor_topic: executor_topic,
       mempool_topic: mempool_topic,
       storage: storage,
       storage_topic: storage_topic
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

    %{
      clock: {nil, %Clock{}},
      configuration:
        {nil, %Anoma.Node.Configuration{configuration: args[:configuration]}},
      logger: {nil, %Logger{}},
      ordering: {nil, %Ordering{}},
      executor: {nil, %Executor{ambiant_env: env}},
      mempool: {nil, %Mempool{block_storage: args[:block_storage]}},
      pinger: {nil, %Pinger{time: args[:ping_time]}},
      storage: {nil, storage},
      storage_data: {storage, block_storage},
      snapshot_path: args[:snapshot_path]
    }
  end

  def state(nodes) do
    GenServer.call(nodes, :state)
  end

  def handle_call(:state, _from, state) do
    {:reply, state, state}
  end

  defp storage_setup(storage, block_storage, rocks) do
    Storage.do_ensure_new(storage, rocks)
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
