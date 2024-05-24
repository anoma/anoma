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

      iex> {:ok, router, _transport} = Router.start()
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
  use Anoma.Node.Router.Engine
  use TypedStruct

  alias __MODULE__
  alias Anoma.Crypto.Id
  alias Anoma.Node.Router.Addr
  alias Anoma.Node.{Transport, Logger}

  @type addr() :: Addr.t()

  @type engine_options() ::
          {:id, Id.t()}
          | {:id, Id.Extern.t()}
          | {:supervisor, Supervisor.supervisor()}
          | {:supvisor_mod, atom()}

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
    field(:local_engines, %{Id.Extern.t() => {Id.t(), GenServer.server()}})
    # mapping of TopicId -> subscriber addrs
    field(:topic_table, %{Id.Extern.t() => MapSet.t(Addr.t())}, default: %{})
    # engine => call number, callee id it's waiting for a response on
    field(
      :waiting_engines,
      %{Id.Extern.t() => {non_neg_integer(), Id.Extern.t()}},
      default: %{}
    )

    # largest id of any outgoing call.  perhaps should be per-engine, or per
    # caller-callee pair (TG points out that not making it per caller-callee
    # pair leaks information; but the alternative leaks space...)
    # perhaps it should be a uuid or some such?
    field(:max_call_id, non_neg_integer(), default: 0)

    # topics to which local engines are subscribed--redundant given the above, but useful
    field(:local_engine_subs, %{addr() => Id.Extern.t()}, default: %{})
    field(:id, Id.Extern.t())
    field(:internal_id, Id.t())
    field(:addr, addr())
    field(:supervisor, atom())
    field(:transport, addr())
    # mapping id -> [pending messages]
    field(:msg_queue, map(), default: %{})
    field(:logger, Addr.t(), enforce: false)
  end

  @spec start_link({Id.t(), addr(), addr(), t()}) :: GenServer.on_start()
  def start_link(init = {_, %{server: name}, _, _}) do
    GenServer.start_link(__MODULE__, init, name: name)
  end

  @spec init({Id.t(), addr(), addr(), t()}) :: {:ok, t()}
  def init({id, router_addr, transport_addr, router_state}) do
    supervisor = process_name(:supervisor, id.external)
    server = process_name(__MODULE__, id.external)

    {:ok,
     %Router{
       router_state
       | id: id.external,
         internal_id: id,
         addr: router_addr,
         transport: transport_addr,
         supervisor: supervisor,
         local_engines:
           Map.merge(router_state.local_engines || %{}, %{
             id.external => {id, server}
           })
     }}
  end

  ############################################################
  #                        Public API                        #
  ############################################################

  @doc """
  Starts a new router with the given cryptographic ID for itself, and another
  for the transport engine.  On success, returns {:ok, router_addr, transport_addr}
  """
  @spec start({Id.t(), Id.t(), Router.t()}) ::
          :ignore
          | {:error, {:already_started, pid()} | :max_children | term()}
          | {:ok, Addr.t(), Addr.t()}
  def start({router_id, transport_id, router_state}) do
    supervisor = process_name(:supervisor, router_id.external)

    {:ok, _} =
      DynamicSupervisor.start_link(
        name: supervisor,
        strategy: :one_for_one,
        max_restarts: 10_000_000,
        max_seconds: 1
      )

    router_name = process_name(__MODULE__, router_id.external)

    router_addr = %Addr{
      id: router_id.external,
      server: router_name,
      router: router_name
    }

    transport_name = process_name(Transport, transport_id.external)

    transport_addr = %Addr{
      id: transport_id.external,
      server: transport_name,
      router: router_name
    }

    with {:ok, _} <-
           DynamicSupervisor.start_child(
             supervisor,
             {__MODULE__,
              {router_id, router_addr, transport_addr, router_state}}
           ),
         {:ok, connection_pool} <-
           DynamicSupervisor.start_child(supervisor, Transport.Supervisor),
         {:ok, _} <-
           DynamicSupervisor.start_child(
             supervisor,
             {Anoma.Node.Router.Engine,
              {router_addr, Transport, transport_id,
               {router_addr, router_id, transport_id, connection_pool}}}
           ) do
      {:ok, router_addr, transport_addr}
    end
  end

  @doc """
  Starts a new router, with a random `Anoma.Crypto.Id`.
  """
  @spec start() :: :ignore | {:error, any()} | {:ok, Addr.t(), Addr.t()}
  def start() do
    start({Id.new_keypair(), Id.new_keypair(), %Router{}})
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

  ### Arguments
  - `router` - the Router
  - `module` - the module we wish to start as an engine
  - `arg` - the argument to startup the engine with
  - `options` - the Options specified by `engine_options()`

  ### Options

  - `id` - An already created ID for the node. This can be an external
  or internal ID.

  """
  @spec start_engine(Addr.t(), atom(), term(), [engine_options()]) ::
          {:ok, Addr.t()}
          | :ignore
          | {:error, {:already_started, pid()} | :max_children | term()}
          # Otherwise start_engine gives a weird error on {:ok, Addr.t()}
          # if we can remove please do
          | any()
  def start_engine(router, module, arg, options \\ []) do
    keys =
      Keyword.validate!(options,
        id: Id.new_keypair(),
        supervisor: call(router, :supervisor),
        supervisor_mod: DynamicSupervisor
      )

    id = keys[:id]
    external = Id.external_id(id)

    with {:ok, _} <-
           keys[:supervisor_mod].start_child(
             keys[:supervisor],
             {Anoma.Node.Router.Engine, {router, module, id, arg}}
           ) do
      {:ok,
       %Addr{
         router: router,
         id: external,
         server: process_name(module, external)
       }}
    end
  end

  @spec start_supervisor(
          Addr.t(),
          Supervisor.child_spec() | {module(), term()}
        ) :: Supervisor.on_start_child()
  def start_supervisor(router, supervisor) do
    DynamicSupervisor.start_child(call(router, :supervisor), supervisor)
  end

  def stop(_router) do
  end

  def set_logger(router, logger) do
    Router.cast(router, {:set_logger, logger})
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
    log_info({:casting, addr})

    # todo message serialisation should happen here (but there is some subtlety
    # because local topics also go through the router, and we don't want to
    # bother serialising anything then)
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

  def call(addr = %Addr{router: router, server: nil}, msg, timeout) do
    log_info({:calling, addr})
    # todo message serialisation should happen here
    GenServer.cast(router, {:call, addr, self_addr(addr), msg, timeout})

    receive do
      # {:router_call_response, err = {:error, :timed_out}} -> exit("timed out")
      # ^^ could do the above if we wanted an out-of-band error instead
      {:router_call_response, err = {:error, :timed_out}} ->
        err

      # todo call-in should pass a specific expected shape for the data, which
      # we can check against
      # and also shape for the message we send out, which we postprocess before
      # sending (isomorphic todos in engine.ex on the other side of this)
      {:router_call_response, res} ->
        case Anoma.Serialise.unpack(res) do
          {:ok, decoded} -> decoded
          _ -> {:error, :message_encoding}
        end
    end
  end

  @doc """
  Send a response to an externally received call.
  This is a low-level routine, intended to be called only by the internals of
  Router and Engine.
  """
  @spec send_response(Addr.t(), Addr.t(), non_neg_integer(), binary()) :: :ok
  def send_response(router, dst, cookie, encoded_response) do
    Router.cast(router, {:send_response, dst, cookie, encoded_response})
  end

  @doc """
  Shuts down the entire router and Elixir node it's in.

  We should tweak this in the future, with a mode to just shut down
  the Anoma node itself.

  """
  def shutdown_node(router) do
    Router.cast(router, :shutdown_everything)
  end

  def dump_state(router) do
    GenServer.call(router, :dump)
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_cast({:init_local_engine, id, server}, s) do
    {:noreply, handle_init_local_engine(id, server, s)}
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

  def handle_cast({:call, addr, src_addr, msg, timeout}, s) do
    {:noreply, do_call(s, addr, src_addr, msg, timeout)}
  end

  def handle_cast({:router_cast, src, msg}, s) do
    {:noreply, handle_self_cast(msg, src, s)}
  end

  def handle_cast({:router_external_cast, src, msg}, s) do
    logger = logger_check(src, s)

    case Anoma.Serialise.unpack(msg) do
      {:ok, msg} ->
        do_handle_external_cast(msg, src, s)

      :error ->
        log_info({:detect_error, msg, logger})
    end
  end

  def handle_self_cast({:set_logger, logger}, _src, s) do
    %Router{s | logger: logger}
  end

  def handle_self_cast(:shutdown_everything, src, s) do
    logger = logger_check(src, s)
    log_info({:shutdown, logger})
    System.stop()
  end

  def handle_self_cast({:send_response, dst, cookie, response}, src, s) do
    send_to_transport(s, dst.id, src.id, [:response, cookie, response])
    s
  end

  def handle_self_cast({:p2p_raw, _node_id, msg}, %{server: server}, s)
      when server != nil do
    case msg do
      %{"data" => data, "sig" => sig} ->
        case Anoma.Serialise.unpack(data) do
          {:ok, [dst_id = %Id.Extern{}, src_id = %Id.Extern{}, msg]} ->
            if not Anoma.Crypto.Sign.verify_detached(sig, data, src_id.sign) do
              log_info({:msg_drop_sig, s.logger})
              s
            else
              src_addr = id_to_addr(s, src_id)
              dst_addr = id_to_addr(s, dst_id)
              logger = logger_check(src_addr, s)

              if not Map.has_key?(s.local_engines, dst_id) do
                log_info({:msg_drop_eng, logger})
                s
              else
                case msg do
                  [:response, call_id, msg] ->
                    # is the engine waiting for a message with this id from this id (todo maybe signs for)?
                    if Map.get(s.waiting_engines, dst_id) ===
                         {call_id, src_id} do
                      send(dst_addr.server, {:router_call_response, msg})

                      %{
                        s
                        | waiting_engines:
                            Map.delete(s.waiting_engines, dst_id)
                      }
                    else
                      # timeout expired, or confused or malicious correspondant; drop

                      log_info({:msg_drop_call, logger})

                      s
                    end

                  [:cast, msg] ->
                    GenServer.cast(
                      dst_addr.server,
                      {:router_external_cast, src_addr, msg}
                    )

                    s

                  [:call, call_id, msg] ->
                    if dst_id === s.id do
                      # message for us? have to handle specially
                      # todo serialisation again
                      send_response(
                        s.addr,
                        src_addr,
                        call_id,
                        Anoma.Serialise.pack(
                          handle_self_call(msg, src_addr, s)
                        )
                      )
                    else
                      GenServer.cast(
                        dst_addr.server,
                        {:router_external_call, src_addr, call_id, msg}
                      )
                    end

                    s

                  # unknown message format; drop
                  _ ->
                    log_info({:msg_drop_format_3, msg, logger})

                    s
                end
              end
            end

          _ ->
            log_info({:msg_drop_format_2, data, s.logger})

            s
        end

      _ ->
        log_info({:msg_drop_format_1, msg, s.logger})

        s
    end
  end

  def handle_call({:router_call, src, msg}, _, s) do
    {res, s} = handle_self_call(msg, src, s)
    {:reply, res, s}
  end

  # TODO Find a better way of dumping
  def handle_call(:dump, _, s) do
    {:reply,
     %__MODULE__{
       s
       | local_engine_subs: %{},
         topic_table: %{},
         waiting_engines: %{},
         msg_queue: %{}
     }, s}
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
    logger = logger_check(src, s)
    log_info({:cast, server, logger})
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

  # send to remote address
  defp do_cast(s, %Addr{id: dst_id}, %Addr{id: src_id}, msg) do
    # todo: serialisation of msg should happen elsewhere
    send_to_transport(s, dst_id, src_id, [:cast, Anoma.Serialise.pack(msg)])
    s
  end

  @spec do_call(t(), Addr.t(), Addr.t(), term(), :erlang.timeout()) :: t()

  # call to an address with a known pid
  defp do_call(s, %Addr{server: server}, src, msg, _timeout)
       when server != nil do
    res = GenServer.call(server, {:router_call, src, msg}, :infinity)
    send(src.server, {:router_call_response, res})
    s
  end

  # call to remote address
  defp do_call(
         s,
         %Addr{id: dst_id},
         src_addr = %Addr{id: src_id},
         msg,
         timeout
       ) do
    # todo: serialisation of msg should happen elsewhere
    send_to_transport(s, dst_id, src_id, [
      :call,
      s.max_call_id,
      Anoma.Serialise.pack(msg)
    ])

    :erlang.send_after(
      timeout,
      self(),
      {:timeout_message, src_addr, s.max_call_id}
    )

    %{
      s
      | max_call_id: 1 + s.max_call_id,
        waiting_engines:
          Map.put(s.waiting_engines, src_id, {s.max_call_id, dst_id})
    }
  end

  def do_handle_external_cast(:shutdown_everything, src, s) do
    logger = logger_check(src, s)
    log_info({:shutdown, logger})
    System.stop()
  end

  def handle_info({:timeout_message, src_addr, call_id}, s) do
    {
      :noreply,
      # we have to check the call id matches; otherwise this could spuriously
      # fail.  there are some other potential ways of dealing with timeouts,
      # but meh
      case Map.get(s.waiting_engines, src_addr.id) do
        {^call_id, _} ->
          send(src_addr.server, {:router_call_response, {:error, :timed_out}})
          %{s | waiting_engines: Map.delete(s.waiting_engines, src_addr.id)}

        _ ->
          s
      end
    }
  end

  defp send_to_transport(s, dst, src, msg) do
    msg = Anoma.Serialise.pack([dst, src, msg])
    {internal_id, _} = Map.fetch!(s.local_engines, src)
    sig = Anoma.Crypto.Sign.sign_detached(msg, internal_id.internal.sign)
    encoded = :msgpack.pack(%{"data" => msg, "sig" => sig})
    Transport.send(s.transport, dst, encoded)
  end

  ############################################################
  #                          Helpers                         #
  ############################################################

  @spec handle_init_local_engine(
          Id.t() | Id.Extern.t(),
          GenServer.server(),
          t()
        ) :: t()
  defp handle_init_local_engine(id = %Id{}, server, s) do
    %{s | local_engines: Map.put(s.local_engines, id.external, {id, server})}
  end

  defp handle_init_local_engine(id = %Id.Extern{}, server, s) do
    %{
      s
      | local_engines:
          if Map.get(s.local_engines, id) do
            s.local_engines
          else
            Map.put(s.local_engines, id, {%Id{external: id}, server})
          end
    }
  end

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

  defp id_to_addr(s, id) do
    %{
      s.addr
      | id: id,
        server: elem(Map.get(s.local_engines, id, {nil, nil}), 1)
    }
  end

  @spec process_name(atom(), Id.Extern.t()) :: atom()
  def process_name(module, id) do
    :erlang.binary_to_atom(
      atom_to_nice_string(module) <> " " <> Base.encode64(id.sign)
    )
  end

  @spec self_addr(Addr.t()) :: Addr.t()
  def self_addr(%Addr{router: router}) do
    %Addr{
      router: router,
      id: Process.get(:engine_id),
      server: Process.get(:engine_server) || self()
    }
  end

  defimpl Inspect, for: Addr do
    def inspect(term, _options) do
      id_string =
        if term.id do
          Inspect.Anoma.Crypto.Id.Extern.inspect(term.id, %Inspect.Opts{})
        else
          "%PUB{}"
        end

      if term.server do
        server =
          if is_pid(term.server) do
            "TASK: " <> (term.server |> :erlang.pid_to_list() |> to_string())
          else
            "MOD: " <>
              (term.server
               |> Atom.to_string()
               |> String.split(" ", trim: true)
               |> hd())
          end

        "%RID{id: #{id_string} #{server}}"
      else
        "%RID{id: #{id_string}}"
      end
    end
  end

  ############################################################
  #                     Logging Info                         #
  ############################################################

  defp logger_check(src, state) do
    logger = state.logger

    if src == logger do
      nil
    else
      logger
    end
  end

  defp log_info({:casting, addr}) do
    Logger.add(nil, :info, "Casting to non-local address #{inspect(addr)}")
  end

  defp log_info({:calling, addr}) do
    Logger.add(nil, :info, "Calling non-local address #{inspect(addr)}")
  end

  defp log_info({:cast, server, logger}) do
    Logger.add(logger, :info, "Message cast to #{inspect(server)}")
  end

  defp log_info({:detect_error, msg, logger}) do
    Logger.add(logger, :error, "Not able to detect message: #{inspect(msg)}")
  end

  defp log_info({:shutdown, logger}) do
    Logger.add(logger, :error, "Shutting down the system")
  end

  defp log_info({:msg_drop_sig, logger}) do
    Logger.add(logger, :debug, "Dropping message; bad signature")
  end

  defp log_info({:msg_drop_eng, logger}) do
    Logger.add(logger, :debug, "Dropping message; unknown engine")
  end

  defp log_info({:msg_drop_call, logger}) do
    Logger.add(
      logger,
      :debug,
      "Dropping message; response but no corresponding call"
    )
  end

  defp log_info({:msg_drop_format_3, msg, logger}) do
    Logger.add(
      logger,
      :debug,
      "Dropping message (3); invalid format #{inspect(msg)}"
    )
  end

  defp log_info({:msg_drop_format_2, data, logger}) do
    Logger.add(
      logger,
      :debug,
      "Dropping message (2); invalid format #{inspect(Anoma.Serialise.unpack(data))}"
    )
  end

  defp log_info({:msg_drop_format_1, msg, logger}) do
    Logger.add(
      logger,
      :debug,
      "Dropping message (1); invalid format #{inspect(msg)}"
    )
  end
end
