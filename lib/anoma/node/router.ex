defmodule Anoma.Node.Router do
  @moduledoc """

  I have a few use cases and APIs throughout an Anoma application:

  1. I provide central routing infrastructure for inter-node and
     intra-node communication

  1. I provide service's for topic creation and
     `Anoma.Node.Router.Engine` creation

  1. I provide a behavior interface similar to `GenServer` for both
     [`Engine`](`Anoma.Node.Router.Engine`) messaging and `Topic`
     messaging


  ## Router Topology

  A good view of my topology and how various
  `Anoma.Node.Router.Engine` and topic's relate to me can be seen by
  the following diagram.

  ```mermaid
  graph TB
    S(Router Supervisor)
    R(Router)
    C(Engine #1) ~~~ B(Engine #2)
    T1(Topic #1) ~~~ T2(Topic #2)
    S ==>R & B & C
    S -.->T1 & T2
    R <-->|router| B & C & T1 & T2
  ```

  `Anoma.Node.Router.Engine`'s that are spawned via `start_engine/3`
  are added to the supervisor, and messages sent via `call/3` or
  `cast/2` may be routed through myself.

  Topics are handled somewhat specially. Since they serve a much more
  limited role, I handle and manage them personally and the dashed
  lines in the diagram are not real. However for building a conceptual
  model, we can view `Topics` spawned by `new_topic/1` as spawning an
  [`Engine`](`Anoma.Node.Router.Engine`), and messages sent via
  `call/3` or `cast/2` as being routed through myself.

  ## Server Terminology

  For the sake of this document, the word `Server` will refer to both
  [`Engines`](`Anoma.Node.Router.Engine`) and `Topics`.

  ## Router PIDs

  The IDs of the various topics and
  [engines](`Anoma.Node.Router.Engine`), are not regular Erlang
  PID's. Instead we use our own address schema laid out in
  `Anoma.Node.Router.Addr`. Further information can also be found in
  our specs (link to specs when they are online).

  ## Router API

  I offer an API for anyone wishing to call me. These functions are:

  - `start/0`
  - `start/1`
  - `new_topic/1`
  - `start_engine/3`
  - `start_engine/4`
  - `subscribe_topic/2`
  - `subscribe_topic/3`

  ## Server APIs

  When writing server code, My module acts as a `GenServer` behavior,
  in that one should be using these functions when writing code to
  talk to the `Server`:

  - `call/2`
  - `call/3`
  - `cast/2`

  The API I offer is very reminiscent of `GenServer`'s API, however
  please read the [examples](#module-examples) section to get a sense
  on how the server code differs.

  ## Client/Server APIs

  Much like `GenServer`, user's typically don't call my `call/3` or
  `cast/2` directly. Instead the user wraps the calls in new functions
  representing the public API of the server. These wrappers are called
  the **client API**.

  ### Examples

  To get a good feel for writing idiomatic code using me, and how my
  API is differs from `GenServer`, we will look at an implementation
  of a stack and see how it [differs from GenServer's stack
  example](https://hexdocs.pm/elixir/GenServer.html#module-client-server-apis)

      defmodule Stack do
        alias Anoma.Node.Router

        use Router.Engine

        @spec init(String.t()) :: {:ok, list(String.t())}
        def init(elements) do
          initial_state = String.split(elements, ",", trim: true)
          {:ok, initial_state}
        end

        @spec push(Router.addr(), String.t()) :: :ok
        def push(pid, element) do
          Router.cast(pid, {:push, element})
        end

        @spec pop(Addr.t()) :: String.t()
        def pop(pid) do
          Router.call(pid, :pop)
        end

        def handle_call(:pop, _from, state) do
          [to_caller | new_state] = state
          {:reply, to_caller, new_state}
        end

        def handle_cast({:push, element}, _from, state) do
          new_state = [element | state]
          {:noreply, new_state}
        end
      end

  And we can use this Engine like the following

      iex> {:ok, router} = Router.start()
      iex> {:ok, stack} = Router.start_engine(router, Stack, "hello")
      iex> Stack.pop(stack)
      "hello"
      iex> Stack.push(stack, "hi")
      :ok
      iex> Stack.pop(stack)
      "hi"

  Overall having both server and client functionality that is clearly
  labeled, leads to good user UI's.

  ### Difference in Design from `GenServer`

  The Stack example shows how my API differs from `Genserver`:

  1. There is currently no `GenServer.start_link/3` for the Router

  1. Even `handle_cast`'s take a `from` parameter

  1. We use `Anoma.Node.Router.Engine` and not `GenServer`

  1. Further `call/2` by default has an :infinite timeout

  ### Summarizing the interactions between Server and Clients

  For basic `Server`'s we can summarize interactions between all
  parties as follows.

    ```mermaid
    sequenceDiagram
      participant C as Client (Process/Engine)
      participant R as Router (Engine)
      participant S as Server (Engine)
      participant M as Module (Code)

      note right of C: Typically started by an init process
      C->>+R: Router.start_engine(router, module, arg, options)
      R->>+S: GenServer.start_link(router, module, arg, options)
      S-->>+M: init(arg)
      M-->>-S: {:ok, state} | :ignore | {:error, reason}
      S->>-R: {:ok, addr} | :ignore | {:error, reason}
      R->>-C: {:ok, addr} | :ignore | {:error, reason}

      note right of C: call is synchronous
      C->>+R: Router.call(addr, message)
      note right of R: For known engines this is optimized away
      R->>+S: GenSever.call(pid, {self, message})
      S-->>+M: handle_call(message, from, state)
      M-->>-S: {:reply, reply, state} | {:stop, reason, reply, state}
      S->>-C: reply

      note right of C: cast is asynchronous
      C-)R: Router.cast(addr, message)
      note right of R: For known engines this is optimized away
      R-)S: GenServer.cast(pid, message)
      S-->>+M: handle_cast(message, state)
      M-->>-S: {:noreply, state} | {:stop, reason, state}

      note right of C: send is asynchronous. the PAID must be known
      C-)S: Kernel.send(pid, message)
      S-->>+M: handle_info(message, state)
      M-->>-S: {:noreply, state} | {:stop, reason, state}
  ```

  What this diagram doesn't show is how Topic messaging works, and for
  that, this diagram may be of assistance.

    ```mermaid
    sequenceDiagram
      participant C as Client (Process/Engine)
      participant S2 as Server2 (Engine)
      participant R as Router (Engine)
      participant T as Topic (Topic)
      participant S as Server (Engine)


      note right of R: For simplicity we do not display a client
      S2-)+R: GenServer.subscribe(router, topic)
      R-->>+T: GenSever.call(addr, {self, message})
      T-->>-R: :ok | {:error, :no_such_topic}
      R-)-S2: :ok | {:error, :no_such_topic}


      note right of C: cast is asynchronous
      C-)R: Router.cast(addr, message)
      R-)+S: GenServer.cast(pid, message)
      note right of S: Imagine this call sends an update to a topic
      S-)-R: Router.cast(topic_addr, msg)
      R-->>T: Genserver.cast(pid, msg)
      T-->>R: Router.cast(addr_S2, msg)
      R-)S2: Genserver.cast(pid, msg)
  ```

  The dotted lines in the diagram are theoretically how `Tasks` could
  be implemented, however this is implementation dependent, and does
  indeed differ from the diagram.

  """
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
    field(:router, Addr.t())
    field(:id, Id.Extern.t() | nil)
    field(:server, GenServer.server() | nil)
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
    field(:supervisor, atom())
    # mapping id -> [pending messages]
    field(:msg_queue, map(), default: %{})
  end

  @spec start_link(Id.t()) :: GenServer.on_start()
  def start_link(id) do
    GenServer.start_link(__MODULE__, id,
      name: process_name(__MODULE__, id.external)
    )
  end

  ############################################################
  #                        Public API                        #
  ############################################################

  @doc """
  Starts a new router with a given cryptographic ID.
  """
  @spec start(Id.t()) ::
          :ignore
          | {:error, {:already_started, pid()} | :max_children | term()}
          | {:ok, Addr.t()}
  def start(id) do
    supervisor = process_name(:supervisor, id.external)

    {:ok, _} =
      DynamicSupervisor.start_link(name: supervisor, strategy: :one_for_one)

    router = process_name(__MODULE__, id.external)

    case DynamicSupervisor.start_child(supervisor, {__MODULE__, id}) do
      {:ok, _} ->
        {:ok,
         %Addr{
           id: id.external,
           server: router,
           router: router
         }}

      err ->
        err
    end
  end

  @doc """
  Starts a new router, with a random `Anoma.Crypto.Id`.
  """
  @spec start() :: :ignore | {:error, any()} | {:ok, Addr.t()}
  def start() do
    start(Id.new_keypair())
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  # not sure exactly how this will work for real, but it's convenient
  # to have for testing right now
  @doc """
  Creates a new topic. Takes the address of the router.
  """
  @spec new_topic(Addr.t()) :: {:ok, Addr.t()} | {:error, :already_exists}
  def new_topic(router) do
    call(router, {:create_topic, Id.new_keypair().external, :local})
  end

  @spec new_topic(Addr.t(), Id.t()) ::
          {:ok, Addr.t()} | {:error, :already_exists}
  def new_topic(router, id) do
    call(router, {:create_topic, id.external, :local})
  end

  @doc """
  Starts a new Engine
  """
  @spec start_engine(Addr.t(), atom(), Id.t(), term()) ::
          {:ok, Addr.t()}
          | :ignore
          | {:error, {:already_started, pid()} | :max_children | term()}
          # Otherwise start_engine gives a weird error on {:ok, Addr.t()}
          # if we can remove please do
          | any()
  def start_engine(router, module, id, arg) do
    with {:ok, _} <-
           DynamicSupervisor.start_child(
             call(router, :supervisor),
             {Anoma.Node.Router.Engine, {router, module, id, arg}}
           ) do
      {:ok,
       %Addr{
         router: router,
         id: id.external,
         server: process_name(module, id.external)
       }}
    end
  end

  # start a new instance of an engine, without caring about the id
  @spec start_engine(Addr.t(), atom(), any()) ::
          {:ok, Addr.t()}
          | :ignore
          | {:error, any()}
          # Otherwise start_engine gives a weird error on {:ok, Addr.t()}
          # if we can remove please do
          | any()
  def start_engine(router, module, arg) do
    start_engine(router, module, Id.new_keypair(), arg)
  end

  def stop(_router) do
  end

  ############################################################
  #                     Public Server API                    #
  ############################################################

  @doc """
  Makes a synchronous call to the `Server` and waits for a reply.

  Call has a few interesting cases we can consider

  1. Casting to a local [`Engine`](`Anoma.Node.Router.Engine`)

  1. Calling to a non local [`Engine`](`Anoma.Node.Router.Engine`)

  1. Casting to a `Topic`


  For the local [`Engine`](`Anoma.Node.Router.Engine`), then their
  [`Engine.handle_cast/3`](`c:Anoma.Node.Router.Engine.handle_cast/3`)
  will be called on the [`Engine`](`Anoma.Node.Router.Engine`) to
  handle the request.

  Calling a non local [`Engine`](`Anoma.Node.Router.Engine`) isn't
  handled yet.

  For `Topics` any `cast/2` sent, then triggers a series of `cast/2`
  to all subscribed `Process`'s

  """
  @spec cast(Addr.t(), term()) :: :ok
  def cast(addr = %Addr{server: server}, msg) when server != nil do
    GenServer.cast(server, {:router_cast, self_addr(addr), msg})
  end

  def cast(addr = %Addr{router: router, server: nil}, msg) do
    Logger.info("casting to non-local addr #{inspect(addr)}")
    GenServer.cast(router, {:cast, addr, self_addr(addr), msg})
  end

  @doc """
  See `call/3` for documentation.
  """
  @spec call(Addr.t(), term()) :: term()
  def call(addr, msg) do
    # default timeout for GenServer.call
    call(addr, msg, 5000)
  end

  @doc """
  Makes a synchronous call to the `Server` and waits for a reply.

  Call has a few interesting cases we can consider

  1. Calling a local [`Engine`](`Anoma.Node.Router.Engine`)

  1. Calling a non local [`Engine`](`Anoma.Node.Router.Engine`)

  We can not `call/3` a `Topic`, and thus those instances are not
  handled.

  Currently we do not handle the non local
  [`Engine`](`Anoma.Node.Router.Engine`) case.

  For the local [`Engine`](`Anoma.Node.Router.Engine`), then their
  [`Engine.handle_call/3`](`c:Anoma.Node.Router.Engine.handle_call/3`)
  will be called on the [`Engine`](`Anoma.Node.Router.Engine`) to
  handle the request.

  ### Timeouts

  By default the timeout is infinite for local calls. For non local
  calls the timeout is not yet defined and will be iterated upon in
  future versions

  """
  @spec call(Addr.t(), term(), :erlang.timeout()) :: term()
  def call(addr = %Addr{server: server}, msg, _timeout) when server != nil do
    # use an infinite timeout for local calls--timeout is only
    # significant for inter-node calls
    GenServer.call(server, {:router_call, self_addr(addr), msg}, :infinity)
  end

  # in this case, rather than the router doing all the work itself, it
  # returns a continuation so we don't bottleneck
  def call(addr = %Addr{router: router, server: nil}, msg, timeout) do
    Logger.info("calling non-local addr #{inspect(addr)}")
    GenServer.call(router, {:call, addr, self_addr(addr), msg, timeout}).()
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

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

  def handle_cast({:cast, addr, src_addr, msg}, s) do
    {:noreply, do_cast(s, addr, src_addr, msg)}
  end

  # def handle_self_cast(_, _, _) when false do
  # end

  # def handle_cast({src, msg}, s) do
  #   {:noreply, handle_self_cast(msg, src, s)}
  # end

  def handle_call({:call, addr, src_addr, msg, timeout}, _, s) do
    {res, s} = do_call(s, addr, src_addr, msg, timeout)
    {:reply, res, s}
  end

  def handle_call({:router_call, src, msg}, _, s) do
    {res, s} = handle_self_call(msg, src, s)
    {:reply, res, s}
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  def handle_self_call(:supervisor, _, s) do
    {s.supervisor, s}
  end

  # create topic.  todo non local topics.  todo the topic should get
  # its own id so distinct topics can be dap
  def handle_self_call({:create_topic, id, :local}, _, s) do
    if Map.has_key?(s.topic_table, id) do
      {{:error, :already_exists}, s}
    else
      {{:ok, %Addr{id: id, router: s.addr.router}},
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
  @spec do_cast(t(), Addr.t(), Addr.t(), term()) :: t()
  defp do_cast(s, %Addr{server: server}, src, msg) when server != nil do
    Logger.info("cast to #{inspect(server)}")
    GenServer.cast(server, {:router_cast, src, msg})
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
  @spec do_call(t(), Addr.t(), Addr.t(), term(), :erlang.timeout()) ::
          {function(), t()}
  defp do_call(s, %Addr{server: server}, src, msg, timeout)
       when server != nil do
    {fn -> GenServer.call(server, {src, msg}, timeout) end, s}
  end

  ############################################################
  #                          Helpers                         #
  ############################################################

  # A module name A.B is represented by an atom with name
  # "Elixir.A.B"; strip away the "Elixir." part
  defp atom_to_nice_string(atom) do
    res = Atom.to_string(atom)

    if String.starts_with?(res, "Elixir.") do
      binary_part(res, 7, byte_size(res) - 7)
    else
      res
    end
  end

  @spec process_name(atom(), Id.Extern.t()) :: atom()
  def process_name(module, id) do
    :erlang.binary_to_atom(
      atom_to_nice_string(module) <> " " <> Base.encode64(id.sign)
    )
  end

  @spec init(Id.t()) :: {:ok, Router.t()}
  def init(id) do
    supervisor = process_name(:supervisor, id.external)
    server = process_name(__MODULE__, id.external)

    {:ok,
     %Router{
       id: id.external,
       addr: %Addr{
         router: server,
         id: id.external,
         server: server
       },
       supervisor: supervisor,
       local_engines: %{id.external => id}
     }}
  end

  @spec self_addr(Addr.t()) :: Addr.t()
  def self_addr(%Addr{router: router}) do
    %Addr{
      router: router,
      id: Process.get(:engine_id),
      server: Process.get(:engine_server) || self()
    }
  end
end
