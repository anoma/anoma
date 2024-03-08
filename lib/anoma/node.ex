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
  """

  use GenServer
  use TypedStruct
  alias Anoma.Node.Router
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

  def start_link(args) do
    snap = args[:snapshot_path]
    storage = args[:storage]

    name = args[:name]
    args = Enum.filter(args, fn {k, _} -> k != :name end)
    resp = GenServer.start_link(__MODULE__, args, name: name)

    unless args[:old_storage] do
      Anoma.Storage.ensure_new(storage)
      Anoma.Storage.put_snapshot(storage, hd(snap))
    end

    resp
  end

  def init(args) do
    env = Map.merge(%Nock{}, Map.intersect(%Nock{}, args |> Enum.into(%{})))
    ping_name = Anoma.Node.Utility.append_name(args[:name], "_pinger")

    {:ok, router} = Router.start()

    {:ok, clock} =
      Router.start_engine(router, Anoma.Node.Clock,
        start: System.monotonic_time(:millisecond)
      )

    {:ok, logger} =
      Router.start_engine(router, Anoma.Node.Logger,
        storage: args[:storage],
        clock: clock
      )

    {:ok, ordering} =
      Router.start_engine(router, Anoma.Node.Storage.Ordering,
        table: args[:storage],
        logger: logger
      )

    env = %{env | ordering: ordering}
    {:ok, executor_topic} = Router.new_topic(router)

    {:ok, executor} =
      Router.start_engine(
        router,
        Anoma.Node.Executor,
        {env, executor_topic, logger}
      )

    {:ok, mempool_topic} = Router.new_topic(router)

    {:ok, mempool} =
      Router.start_engine(router, Anoma.Node.Mempool,
        block_storage: args[:block_storage],
        ordering: ordering,
        executor: executor,
        topic: mempool_topic,
        logger: logger
      )

    {:ok, pinger} =
      Router.start_engine(router, Anoma.Node.Pinger,
        name: ping_name,
        mempool: mempool,
        time: args[:ping_time]
      )

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

  def state(nodes) do
    GenServer.call(nodes, :state)
  end

  def handle_call(:state, _from, state) do
    {:reply, state, state}
  end
end
