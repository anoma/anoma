defmodule Anoma.Node.Router do
  use GenServer
  use TypedStruct
  require Logger

  alias __MODULE__
  alias Anoma.Crypto.Id
  alias Anoma.Node.Router.Addr

  @type addr() :: Addr.t()

  typedstruct module: Addr do
    @moduledoc """
    An address to which we can send a message.
    The server, if known, is a local actor which can receive it directly;
      otherwise, the mssage will be sent via the central router.
    If the server is known, but the id is not, then this is a local-only
      engine, which can only talk to other local engines.
    (Hence, at least one of id and server must be known; potentially both are.)
    """
    field(:router, GenServer.server())
    # :erlang.binary_to_atom("registry " <> router.sign, :latin1)
    field(:registry, atom())

    field(:id, Id.Extern.t())
    field(:server, GenServer.server())
  end

  typedstruct do
    # slightly space-inefficient (duplicates extern), but more convenient
    field(:local_engines, %{Id.Extern.t() => Id.t()})
    # mapping of TopicId -> subscriber addrs
    field(:topic_table, %{Id.Extern.t() => MapSet.t(Addr.t())}, default: %{})

    # topics to which local engines are subscribed--redundant given the above, but useful
    field(:local_engine_subs, %{addr() => Id.Extern.t()}, default: %{})
    field(:id, Id.Extern.t())
    field(:addr, addr())
    field(:registry, atom())
    field(:supervisor, atom())
    # mapping id -> [pending messages]
    field(:msg_queue, map(), default: %{})
  end

  def registry_name(id) do
    :erlang.binary_to_atom("registry " <> id.sign, :latin1)
  end

  def supervisor_name(id) do
    :erlang.binary_to_atom("supervisor " <> id.sign, :latin1)
  end

  def start_link(id) do
    GenServer.start_link(__MODULE__, id,
      name: {:via, Registry, {registry_name(id.external), id.external}}
    )
  end

  def start(id) do
    supervisor = supervisor_name(id.external)
    registry = registry_name(id.external)

    {:ok, _} =
      DynamicSupervisor.start_link(name: supervisor, strategy: :one_for_one)

    {:ok, _} =
      DynamicSupervisor.start_child(
        supervisor,
        {Registry, keys: :unique, name: registry}
      )

    # {:ok, _} = Registry.start_link(keys: :unique, name: __MODULE__.Registry)
    router = {:via, Registry, {registry, id.external}}

    case DynamicSupervisor.start_child(supervisor, {__MODULE__, id}) do
      {:ok, _} ->
        {:ok,
         %Addr{
           id: id.external,
           registry: registry,
           server: router,
           router: router
         }}

      err ->
        err
    end
  end

  def start() do
    start(Id.new_keypair())
  end

  def stop(_router) do
  end

  def init(id) do
    supervisor = supervisor_name(id.external)
    registry = registry_name(id.external)
    server = {:via, Registry, {registry, id.external}}

    {:ok,
     %Router{
       id: id.external,
       addr: %Addr{
         router: server,
         registry: registry,
         id: id.external,
         server: server
       },
       supervisor: supervisor,
       registry: registry,
       local_engines: %{id.external => id}
     }}
  end

  # public interface
  def cast(addr = %Addr{server: server}, msg) when server != nil do
    GenServer.cast(server, {self_addr(addr), msg})
  end

  def cast(addr = %Addr{router: router, server: nil}, msg) do
    Logger.info("casting to non-local addr #{inspect(addr)}")
    GenServer.cast(router, {:cast, addr, self(), msg})
  end

  # default timeout for GenServer.call
  def call(addr, msg) do
    call(addr, msg, 5000)
  end

  def call(addr = %Addr{server: server}, msg, timeout) when server != nil do
    GenServer.call(server, {self_addr(addr), msg}, timeout)
  end

  # in this case, rather than the router doing all the work itself, it
  # returns a continuation so we don't bottleneck
  def call(addr = %Addr{router: router, server: nil}, msg, timeout) do
    Logger.info("calling non-local addr #{inspect(addr)}")
    GenServer.call(router, {:call, addr, msg, timeout}).()
  end

  def self_addr(ctx) do
    pid_to_addr(ctx, self())
  end

  def pid_to_addr(%Router{addr: addr}, pid) do
    pid_to_addr(addr, pid)
  end

  def pid_to_addr(%Addr{registry: registry, router: router}, pid) do
    case Registry.lookup(registry, pid) do
      [{_, id}] ->
        %Addr{
          id: id,
          registry: registry,
          router: router,
          server: {:via, Registry, {registry, id}}
        }

      [] ->
        %Addr{server: pid}
    end
  end

  # not sure exactly how this will work for real, but it's convenient
  # to have for testing right now
  def new_topic(router) do
    call(router, {:create_topic, Id.new_keypair().external, :local})
  end

  def start_engine(router, module, id, arg) do
    # case Anoma.Node.Router.Engine.start_link({module, id, arg}) do
    case DynamicSupervisor.start_child(
           call(router, :supervisor),
           {Anoma.Node.Router.Engine, {router, module, id, arg}}
         ) do
      {:ok, pid} -> {:ok, pid_to_addr(router, pid)}
      err -> err
    end
  end

  # start a new instance of an engine, without caring about the id
  def start_engine(router, module, arg) do
    start_engine(router, module, Id.new_keypair(), arg)
  end

  def handle_cast({:init_local_engine, id, _pid}, s) do
    s = %{s | local_engines: Map.put(s.local_engines, id.external, id)}
    {:noreply, s}
  end

  def handle_cast({:cleanup_local_engine, addr}, s) do
    s = %{
      s
      | local_engines: Map.delete(s.local_engines, addr.id),
        # remove all this engine's registrations
        topic_table:
          Enum.reduce(
            Map.get(s.local_engine_subs, addr, MapSet.new()),
            s.topic_table,
            fn topic, table ->
              Map.update!(table, topic, fn subscribers ->
                MapSet.delete(subscribers, addr)
              end)
            end
          ),
        local_engine_subs: Map.delete(s.local_engine_subs, addr)
    }

    {:noreply, s}
  end

  def handle_cast({:cast, addr, pid, msg}, s) do
    {:noreply, do_cast(s, addr, pid_to_addr(s, pid), msg)}
  end

  # def handle_self_cast(_, _, _) when false do
  # end

  # def handle_cast({src, msg}, s) do
  #   {:noreply, handle_self_cast(msg, src, s)}
  # end

  def handle_call({:call, addr, msg, timeout}, {pid, _}, s) do
    {res, s} = do_call(s, addr, pid_to_addr(s, pid), msg, timeout)
    {:reply, res, s}
  end

  def handle_call({src, msg}, _, s) do
    {res, s} = handle_self_call(msg, src, s)
    {:reply, res, s}
  end

  def handle_self_call(:supervisor, _, s) do
    {s.supervisor, s}
  end

  # create topic.  todo non local topics.  todo the topic should get
  # its own id so distinct topics can be dap
  def handle_self_call({:create_topic, id, :local}, _, s) do
    if Map.has_key?(s.topic_table, id) do
      {{:error, :already_exists}, s}
    else
      {{:ok, %Addr{id: id, router: s.addr.router, registry: s.registry}},
       %{s | topic_table: Map.put(s.topic_table, id, MapSet.new())}}
    end
  end

  # subscribe to topic
  # be nice and treat an address interchangeably
  # with an id (probably at some point this will be the only way to do
  # node-local topics)
  def handle_self_call({:subscribe_topic, %Addr{id: id}, scope}, addr, s) do
    handle_self_call({:subscribe_topic, id, scope}, addr, s)
  end

  def handle_self_call({:subscribe_topic, topic, :local}, addr, s) do
    if Map.has_key?(s.topic_table, topic) do
      s = %{
        s
        | topic_table:
            Map.update!(s.topic_table, topic, fn d -> MapSet.put(d, addr) end),
          local_engine_subs:
            Map.update(s.local_engine_subs, addr, MapSet.new([topic]), fn s ->
              MapSet.put(s, topic)
            end)
      }

      {:ok, s}
    else
      {{:error, :no_such_topic}, s}
    end
  end

  # unsubscribe.  todo should this error if the topic exists but
  # they're not already subscribed?
  def handle_self_call({:unsubscribe_topic, %Addr{id: id}, scope}, addr, s) do
    handle_self_call({:unsubscribe_topic, id, scope}, addr, s)
  end

  def handle_self_call({:unsubscribe_topic, topic, :local}, addr, s) do
    if Map.has_key?(s.topic_table, topic) do
      s = %{
        s
        | topic_table:
            Map.update!(s.topic_table, topic, fn d ->
              MapSet.delete(d, addr)
            end),
          local_engine_subs:
            Map.update!(s.local_engine_subs, addr, fn s ->
              MapSet.delete(s, topic)
            end)
      }

      {:ok, s}
    else
      {{:error, :no_such_topic}, s}
    end
  end

  # send to an address with a known pid
  defp do_cast(s, %Addr{server: server}, src, msg) when server != nil do
    Logger.info("cast to #{inspect(server)}")
    GenServer.cast(server, {src, msg})
    s
  end

  # send to a topic we know about
  defp do_cast(s, %Addr{id: id}, src, msg)
       when is_map_key(s.topic_table, id) do
    Enum.reduce(Map.get(s.topic_table, id), s, fn recipient, s ->
      do_cast(s, recipient, src, msg)
    end)
  end

  # call to an address with a known pid
  defp do_call(s, %Addr{server: server}, src, msg, timeout)
       when server != nil do
    {fn -> GenServer.call(server, {src, msg}, timeout) end, s}
  end
end
