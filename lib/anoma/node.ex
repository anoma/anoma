defmodule Anoma.Node do
  @moduledoc """
  I am the supervisor for Anoma Nodes

  ## Required Arguments

    - `name` - name to register the nodes
    - `snapshot_path` : [`atom()` | 0]
      - A snapshot location for the service (used in the worker)
    - `storage` : `Anoma.Storage.t()` - The Storage tables to use
    - `block_storage` - a location to store the blocks produced
  ## Optional Arguments

    - `jet` : `Nock.jettedness()` - how jetted the system should be
    - `old_storage` : `boolean` - states if the storage should be freshly made
       - by default it is `false`
  ## Registered names

  From the given `name` argument we derive the following:
    - `name_mempool`
    - `name_executor`
    - `name_ordering`
    - `name_dynamic`
    - `name_measure`
    - `name_mempool_com`
    - `name_executor_com`
    - `name_ordering_com`
    - `name_dynamic_com`
    - `name_measure_com`

  ### Created Tables
    - `storage.qualified`
    - `storage.order`
    - `block_storage`
  """

  use Supervisor
  use TypedStruct
  alias Anoma.Node.Utility
  alias __MODULE__

  typedstruct enforce: true do
    field(:mempool, GenServer.server())
    field(:ordering, GenServer.server())
    field(:executor, GenServer.server())
    field(:dynamic, GenServer.server())
    field(:measure, GenServer.server())
  end

  def start_link(args) do
    snap = args[:snapshot_path]
    storage = args[:storage]

    coms = com_names(args[:name])
    prims = names(args[:name])
    args = args |> Keyword.put(:ordering, coms.ordering)

    resp = Supervisor.start_link(__MODULE__, {coms, prims, args})

    unless args[:old_storage] do
      Anoma.Storage.ensure_new(storage)
      Anoma.Storage.put_snapshot(storage, hd(snap))
    end

    resp
  end

  def init({coms, prims, args}) do
    env = Map.merge(%Nock{}, Map.intersect(%Nock{}, args |> Enum.into(%{})))
    storage = args[:storage]

    children = [
      {Anoma.Node.Executor,
       env |> Map.to_list() |> Keyword.put(:name, prims.executor)},
      {Anoma.Node.Storage, name: prims.ordering, table: storage},
      {Anoma.Node.Mempool,
       name: prims.mempool,
       block_storage: args[:block_storage],
       ordering: coms.ordering,
       executor: coms.executor},
      {Anoma.Node.Control,
       dyn: prims.dynamic, meas: prims.measure, storage: storage}
    ]

    Supervisor.init(children, strategy: :one_for_all)
  end

  @spec names(atom()) :: Node.t()
  def names(name) do
    exec = Utility.append_name(name, "_executor")
    mem = Utility.append_name(name, "_mempool")
    ord = Utility.append_name(name, "_ordering")
    dyn = Utility.append_name(name, "_dynamic")
    meas = Utility.append_name(name, "_measure")

    %Node{
      mempool: mem,
      ordering: ord,
      executor: exec,
      dynamic: dyn,
      measure: meas
    }
  end

  @spec com_names(atom()) :: Node.t()
  def com_names(name) do
    exec = Utility.append_name(name, "_executor_com")
    mem = Utility.append_name(name, "_mempool_com")
    ord = Utility.append_name(name, "_ordering_com")
    dyn = Utility.append_name(name, "_dynamic_com")
    meas = Utility.append_name(name, "_measure_com")

    %Node{
      mempool: mem,
      ordering: ord,
      executor: exec,
      dynamic: dyn,
      measure: meas
    }
  end

  def shutdown(supervisor), do: Supervisor.stop(supervisor, :normal)
end
