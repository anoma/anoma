defmodule Anoma.Node do
  @moduledoc """
  I act as a registry for Anoma Nodes

  ## Required Arguments

    - `name` - name for this process
    - `snapshot_path` : [`atom()` | 0]
      - A snapshot location for the service (used in the worker)
    - `storage` : `Anoma.Storage.t()` - The Storage tables to use
    - `block_storage` - a location to store the blocks produced
  ## Optional Arguments

    - `jet` : `Nock.jettedness()` - how jetted the system should be
    - `old_storage` : `boolean` - states if the storage should be freshly made
       - by default it is `false`
  ## Registered names

  ### Created Tables
    - `storage.qualified`
    - `storage.order`
    - `block_storage`

  I also can receive full Node info in the format specified in Anoma.Dump
  via the `load` function. In this case, the arguments to start me require

  """

  use GenServer
  use TypedStruct
  alias Anoma.Node.{Router, Logger, Clock, Executor, Mempool, Pinger}
  alias Anoma.Node.Storage.Ordering
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
  end

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

  def start_link(args) do
    settings = args[:settings]
    name = args[:name]
    {storage, block_storage} = settings[:storage]
    storage_setup(storage, block_storage)

    if args[:new_storage] do
      snap = settings[:snapshot_path]

      Anoma.Storage.put_snapshot(storage, hd(snap))
    else
      tables =
        settings[:qualified] ++ settings[:order] ++ settings[:block_storage]

      tables
      |> Enum.map(fn x ->
        fn -> :mnesia.write(x) end |> :mnesia.transaction()
      end)
    end

    GenServer.start_link(__MODULE__, settings, name: name)
  end

  def init(args) do
    {log_id, log_st} = args[:logger]
    {clock_id, _clock_st} = args[:clock]
    {ord_id, ord_st} = args[:ordering]
    {mem_id, mem_st} = args[:mempool]
    {ping_id, ping_st} = args[:pinger]
    {ex_id, ex_st} = args[:executor]

    {:ok, router} = start_router(args[:router])

    {:ok, clock} =
      start_engine(router, Clock, clock_id,
        start: System.monotonic_time(:millisecond)
      )

    {:ok, logger} =
      start_engine(
        router,
        Logger,
        log_id,
        %Logger{log_st | clock: clock}
      )

    {:ok, ordering} =
      start_engine(
        router,
        Ordering,
        ord_id,
        %Ordering{ord_st | logger: logger}
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
       mempool_topic: mempool_topic
     }}
  end

  def start_min(args) do
    env = Map.merge(%Nock{}, Map.intersect(%Nock{}, args |> Enum.into(%{})))
    storage = args[:storage]
    block_storage = args[:block_storage]

    %{
      clock: {nil, %Clock{}},
      logger: {nil, %Logger{storage: storage}},
      ordering: {nil, %Ordering{table: storage}},
      executor: {nil, %Executor{ambiant_env: env}},
      mempool: {nil, %Mempool{block_storage: args[:block_storage]}},
      pinger: {nil, %Pinger{time: args[:ping_time]}},
      storage: {storage, block_storage},
      snapshot_path: args[:snapshot_path]
    }
  end

  def state(nodes) do
    GenServer.call(nodes, :state)
  end

  def handle_call(:state, _from, state) do
    {:reply, state, state}
  end

  defp storage_setup(storage, block_storage) do
    Anoma.Storage.ensure_new(storage)
    :mnesia.delete_table(block_storage)
    Anoma.Block.create_table(block_storage, false)
  end

  defp start_router(router) do
    if router == nil do
      Router.start()
    else
      Router.start(router)
    end
  end

  defp start_engine(router, module, id, state) do
    if id == nil do
      Router.start_engine(router, module, state)
    else
      Router.start_engine(router, module, id, state)
    end
  end

  defp new_topic(router, id) do
    if id == nil do
      Router.new_topic(router)
    else
      Router.new_topic(router, id)
    end
  end
end
