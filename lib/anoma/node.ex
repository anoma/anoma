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
    field(:ordering, Router.addr())
    field(:executor, Router.addr())
    field(:executor_topic, Router.addr())
    field(:mempool, Router.addr())
    field(:mempool_topic, Router.addr())
    field(:pinger, Router.addr())
    field(:clock, Router.addr())
    field(:logger, Router.addr())
    field(:storage, Router.addr())
  end

  @type configuration() :: [
          new_storage: boolean(),
          name: atom(),
          use_rocks: boolean(),
          settings: engine_configuration()
        ]

  @type min_engine_configuration() :: [
          ping_time: :no_timer | non_neg_integer(),
          name: atom(),
          snapshot_path: Noun.t(),
          storage_data: Storage.t(),
          block_storage: atom()
        ]

  @type engine_configuration() :: %{
          clock: {Router.addr() | nil, Clock.t()},
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

      {:ok, pid}
    end
  end

  def init(args) do
    {log_id, log_st} = args[:logger]
    {clock_id, _clock_st} = args[:clock]
    {ord_id, ord_st} = args[:ordering]
    {mem_id, mem_st} = args[:mempool]
    {ping_id, ping_st} = args[:pinger]
    {ex_id, ex_st} = args[:executor]
    {storage_id, storage_st} = args[:storage]

    {:ok, router} = start_router(args[:router])

    {:ok, storage} =
      start_engine(
        router,
        Storage,
        storage_id,
        %Storage{storage_st | namespace: [router.id]}
      )

    {:ok, clock} =
      start_engine(router, Clock, clock_id,
        start: System.monotonic_time(:millisecond)
      )

    {:ok, logger} =
      start_engine(
        router,
        Logger,
        log_id,
        %Logger{log_st | clock: clock, storage: storage}
      )

    {:ok, ordering} =
      start_engine(
        router,
        Ordering,
        ord_id,
        %Ordering{ord_st | logger: logger, table: storage}
      )

    {:ok, executor_topic} = new_topic(router, args[:executor_topic])

    {:ok, executor} =
      start_engine(
        router,
        Executor,
        ex_id,
        %Executor{
          ex_st
          | logger: logger,
            task_completion_topic: executor_topic,
            ambiant_env: %Nock{
              ex_st.ambiant_env
              | logger: logger,
                ordering: ordering
            }
        }
      )

    {:ok, mempool_topic} = new_topic(router, args[:mempool_topic])

    {:ok, mempool} =
      start_engine(router, Mempool, mem_id, %Mempool{
        mem_st
        | logger: logger,
          topic: mempool_topic,
          ordering: ordering,
          executor: executor
      })

    {:ok, pinger} =
      start_engine(router, Pinger, ping_id, %Pinger{
        ping_st
        | mempool: mempool
      })

    Anoma.Node.Pinger.start(pinger)

    {:ok,
     %Node{
       router: router,
       ordering: ordering,
       executor: executor,
       mempool: mempool,
       pinger: pinger,
       clock: clock,
       logger: logger,
       executor_topic: executor_topic,
       mempool_topic: mempool_topic,
       storage: storage
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

  defp start_router(router) do
    if router == nil do
      Router.start()
    else
      Router.start(%Id{external: router})
    end
  end

  defp start_engine(router, module, id, state) do
    if id == nil do
      Router.start_engine(router, module, state)
    else
      Router.start_engine(router, module, %Id{external: id}, state)
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
