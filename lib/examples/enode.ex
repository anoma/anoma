defmodule Examples.ENode do
  @moduledoc """
  I give examples of the ENode.

  Currently I make networks in `Phase_1` of the example. Meaning that
  network configurations I provide, are not cloned. The given storage
  names will be written over for any shared set of storage.

  When `phase_2` of the changes come in, the signature and examples of
  this module in particular will likely change quite a bit to return
  persistent networks that other examples can use
  non-destructively. And thus every example can simply add on to
  another. However since we don't have this examples, are expected to
  clean out the state before running.
  """
  use Memoize

  require ExUnit.Assertions
  import ExUnit.Assertions

  alias Anoma.Node.Transport
  alias Anoma.Node.Solver
  alias Anoma.Node.IntentPool
  alias Anoma.Configuration
  alias Anoma.Node
  alias Anoma.Node.{Router, Ordering, Storage}

  @doc """
  We give an example of the full node!
  """
  @spec fresh_full_node(Storage.t(), atom()) :: Node.t()
  @spec fresh_full_node(
          Storage.t(),
          atom(),
          Configuration.configuration_map() | nil
        ) :: Node.t()
  def fresh_full_node(storage, name, configuration \\ nil) do
    config =
      if configuration do
        [configuration: configuration]
      else
        []
      end

    options = [
      snapshot_path: base_snapshot_path(),
      storage_data: storage,
      block_storage: name,
      ping_time: :no_timer
    ]

    {:ok, nodes} =
      Anoma.Node.start_link_or_find_instance(
        testing: true,
        use_rocks: false,
        settings: {:new_storage, (options ++ config) |> Node.start_min()}
      )

    Node.state(nodes)
  end

  @spec attach_socks(Node.t()) :: {Node.t(), Transport.transport_addr()}
  @spec attach_socks(Node.t(), Keyword.t()) ::
          {Node.t(), Transport.transport_addr()}
  def attach_socks(node, options \\ []) do
    assert {:ok, keys} = Keyword.validate(options, cleanup: true)

    socket_name = (:enacl.randombytes(8) |> Base.encode32()) <> ".sock"

    socket_path = Anoma.System.Directories.data(socket_name)
    socket_addr = {:unix, socket_path}

    Transport.start_server(node.transport, socket_addr)

    assert :pong == Router.call(node.transport, :ping)

    if keys[:cleanup] do
      File.rm(socket_path)
    end

    {node, socket_addr}
  end

  @spec solver(Node.t()) ::
          {Router.addr(), Router.addr(), Router.addr(), Node.t()}
  def solver(node) do
    {it, ip, node} = intent(node)

    assert {:ok, solutions} = Router.new_topic(node.router)

    assert {:ok, solver} =
             Router.start_engine(
               node.router,
               Solver,
               {node.router, nil, ip, it, solutions}
             )

    {solver, ip, solutions, node}
  end

  @spec intent(Node.t()) :: {Router.addr(), Router.addr(), Node.t()}
  def intent(node) do
    assert {:ok, intent_topic} = Router.new_topic(node.router)

    assert {:ok, intent_pool} =
             Router.start_engine(node.router, IntentPool, {intent_topic, nil})

    {intent_topic, intent_pool, node}
  end

  @doc """
  Simple node with the minimal dependencies for Storage
  """
  @spec simple_ordering(Storage.t()) :: Node.t()
  def simple_ordering(storage) do
    node = %Node{} = simple_storage(storage)

    assert {:ok, ordering} =
             Router.start_engine(node.router, Ordering, storage: node.storage)

    %Node{node | ordering: ordering}
  end

  # TODO we should resuse `simple_storage/1`, but we need to pass in the
  # storage at startup sadly
  @doc """
  Minimal node, with storage and a topic
  """
  @spec simple_storage_topic(Storage.t()) :: Node.t()
  def simple_storage_topic(storage) do
    node = simple_router()
    assert {:ok, topic} = Router.new_topic(node.router)
    storage = %Storage{storage | topic: topic}
    assert {:ok, storage} = Router.start_engine(node.router, Storage, storage)
    %Node{node | storage: storage, storage_topic: topic}
  end

  @doc """
  Minimal node, with storage
  """
  @spec simple_storage(Storage.t()) :: Node.t()
  def simple_storage(storage) do
    node = simple_router()
    assert {:ok, storage} = Router.start_engine(node.router, Storage, storage)
    %Node{node | storage: storage}
  end

  @doc """
  I am just the clock engine with router support. My time starts at 0
  """
  def zero_clock() do
    node = simple_router()

    assert {:ok, clock} =
             Router.start_engine(node.router, Anoma.Node.Clock, start: 0)

    %Node{node | clock: clock}
  end

  @doc """
  I am simply a node with just a router and transport
  """
  @spec simple_router() :: Node.t()
  def simple_router() do
    assert {:ok, router, transport} = Router.start()
    %Node{router: router, transport: transport}
  end

  @spec base_snapshot_path() :: Noun.t()
  def base_snapshot_path() do
    # My special snapshot isn't popular anymore
    [999_888_777_666 | 0]
  end
end
