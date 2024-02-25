defmodule Anoma.Node.Router do
  use GenServer
  use TypedStruct
  require Logger

  alias __MODULE__
  alias Anoma.Crypto.Id

  typedstruct module: Addr do
    @moduledoc """
    An address to which we can send a message.
    The pid, if known, is a local actor which can receive it directly;
      otherwise, the mssage will be sent via the central router.
    If the pid is known, but the id is not, then this is a local-only
      engine, which can only talk to other local engines.
    (The router's pid is included for convenience.)
    """
    field(:router, pid())
    field(:id, Id.Extern.t())
    field(:pid, pid())
  end

  typedstruct do
    # slightly space-inefficient (duplicates extern), but more convenient
    field(:local_engines, %{Id.Extern.t() => {Id.t(), Addr.t()}})
    # mapping of pid -> Addr for all local engines
    field(:local_engine_addrs, %{pid() => Addr.t()})
    # mapping of TopicId -> subscriber addrs
    field(:topic_table, %{Id.Extern.t() => [Addr.t()]}, default: %{})
    # names of the transport engine and network identity store
    field(:id, Id.Extern.t())
    # mapping id -> [pending messages]
    field(:msg_queue, Map.t(), default: %{})
  end

  def start_link(id) do
    case GenServer.start_link(__MODULE__, id) do
      {:ok, pid} -> {:ok, %Addr{router: pid, id: id.external, pid: pid}}
      err -> err
    end
  end
  def start_link() do
    start_link(Id.new_keypair())
  end
  def init(id) do
    addr = %Addr{router: self(), id: id.external, pid: self()}
    {:ok,
      %Router{
        id: id.external,
        local_engines: %{id.external => {id,addr}},
        local_engine_addrs: %{self() => addr}}}
  end

  # public interface
  def cast(%Addr{router: router, pid: pid}, msg) when router === pid do
    GenServer.cast(router, msg)
  end
  def cast(%Addr{router: router, pid: pid}, msg) when pid != nil do
    GenServer.cast(pid, {%Addr{router: router, pid: self()}, msg})
  end
  def cast(addr = %Addr{router: router, pid: nil}, msg) do
    Logger.info("casting to non-local addr #{inspect(addr)}")
    GenServer.cast(router, {:cast, addr, self(), msg})
  end

  def call(addr, msg) do call(addr, msg, 5000) end # default timeout for GenServer.call
  def call(%Addr{router: router, pid: pid}, msg, timeout) when router === pid do
    GenServer.call(router, msg, timeout)
  end
  def call(%Addr{router: router, pid: pid}, msg, timeout) when pid != nil do
    GenServer.call(pid, {%Addr{router: router, pid: self()}, msg}, timeout)
  end
  # in this case, rather than the router doing all the work itself, it returns a continuation so we don't bottleneck
  def call(addr = %Addr{router: router, pid: nil}, msg, timeout) do
    Logger.info("calling non-local addr #{inspect(addr)}")
    GenServer.call(router, {:call, addr, msg, timeout}).()
  end

  # not sure exactly how this will work for real, but it's convenient to have for testing right now
  def new_topic(router) do
    GenServer.call(router.router, {:create_topic, Id.new_keypair().external, :local})
  end

  # GenServer wrapper to let us interpose some communication before the child process starts, and some wrapping of message receipt
  defmodule Engine do
    use GenServer
    def init({router, mod, id, arg}) do
      GenServer.cast(router, {:init_local_engine, id, self()})
      case mod.init(arg) do
        {:ok, state} -> {:ok, {mod, state}}
        err -> err
      end
    end
    def handle_cast({src, msg}, {mod, state}) do
      {:noreply, state} = mod.handle_cast(msg, src, state)
      {:noreply, {mod, state}}
    end
    def handle_call({src, msg}, _, {mod, state}) do
      case mod.handle_call(msg, src, state) do
        {:reply, res, state} -> {:reply, res, {mod, state}}
        {:reply, res, state, cont = {:continue, _}} -> {:reply, res, {mod, state}, cont}
      end
    end
    def handle_continue(arg, {mod, state}) do
      {:noreply, state} = mod.handle_continue(arg, state)
      {:noreply, {mod, state}}
    end
    def handle_info(info, {mod, state}) do
      {:noreply, state} = mod.handle_info(info, state)
      {:noreply, {mod, state}}
    end
  end

  def start_engine(router, module, id, arg) do
    case GenServer.start_link(Engine, {router.router, module, id, arg}) do
      {:ok, pid} -> {:ok, %Addr{router: router.router, id: id.external, pid: pid}}
      err -> err
    end
  end
  # start a new instance of an engine, without caring about the id
  def start_engine(router, module, arg) do
    start_engine(router, module, Id.new_keypair(), arg)
  end

  def handle_cast({:init_local_engine, id, pid}, s) do
    addr = %Addr{router: self(), pid: pid, id: id.external}
    s = %{s |
      local_engines: Map.put(s.local_engines, id.external, {id, addr}),
      local_engine_addrs: Map.put(s.local_engine_addrs, pid, addr)}
    {:noreply, s}
  end

  def handle_cast({:cast, addr, pid, msg}, s) do
    {:noreply, do_cast(s, addr, pid_to_addr(s, pid), msg)}
  end
  def handle_call({:call, addr, msg, timeout}, {pid, _}, s) do
    {res, s} = do_call(s, addr, pid_to_addr(s, pid), msg, timeout)
    {:reply, res, s}
  end

  # create topic.  todo non local topics.  todo the topic should get its own id so distinct topics can be dap
  def handle_call({:create_topic, id, :local}, _, s) do
    if Map.has_key?(s.topic_table, id) do
      {:reply, {:error, :already_exists}, s}
    else
      {:reply,
        {:ok, %Addr{router: self(), id: id}},
        %{s | topic_table: Map.put(s.topic_table, id, MapSet.new)}}
    end
  end

  # subscribe to topic
  def handle_call({:subscribe_topic, topic, :local}, {pid, _}, s) do
    if Map.has_key?(s.topic_table, topic) do
      s = %{s | topic_table: Map.update!(s.topic_table, topic,
        fn d -> MapSet.put(d, pid_to_addr(s, pid)) end)}
      {:reply, :ok, s}
    else 
      {:reply, {:error, :no_such_topic}, s}
    end
  end
  # unsubscribe.  todo should this error if the topic exists but they're not already subscribed?
  def handle_call({:unsubscribe_topic, topic, :local}, {pid, _}, s) do
    if Map.has_key?(s.topic_table, topic) do
      s = %{s | topic_table: Map.update!(s.topic_table, topic,
        fn d -> MapSet.delete(d, pid_to_addr(s, pid)) end)}
      {:reply, :ok, s}
    else
      {:reply, {:error, :no_such_topic}, s}
    end
  end

  def handle_call(:self, {pid, _}, s) do
    {:reply, pid_to_addr(s, pid), s}
  end

  # send to an address with a known pid
  defp do_cast(s, %Addr{pid: pid}, src, msg) when pid != nil do
    Logger.info("cast to #{inspect(pid)}")
    GenServer.cast(pid, {src, msg})
    s
  end

  # send to a topic we know about
  defp do_cast(s, %Addr{id: id}, src, msg) when is_map_key(s.topic_table, id) do
    Enum.reduce(Map.get(s.topic_table, id), s,
      fn recipient, s -> do_cast(s, recipient, src, msg) end)
  end

  # send to a local engine
  defp do_cast(s, %Addr{id: id}, src, msg) when is_map_key(s.local_engines, id) do
    with {_, pid} <- Map.fetch!(s.local_engines, id) do
      GenServer.cast(pid, {src, msg})
    end
  end

  # call to an address with a known pid
  defp do_call(s, %Addr{pid: pid}, src, msg, timeout) when pid != nil do
    {fn -> GenServer.call(pid, {src, msg}, timeout) end, s}
  end

  # call to a local engine
  defp do_call(s, %Addr{id: id}, src, msg, timeout) when is_map_key(s.local_engines, id) do
    with {_, pid} <- Map.fetch!(s.local_engines, id) do
      {fn -> GenServer.call(pid, {src, msg}, timeout) end, s}
    end
  end

  defp pid_to_addr(s, pid) do
    Map.get(s.local_engine_addrs, pid, %Addr{router: self(), pid: pid})
  end
end
