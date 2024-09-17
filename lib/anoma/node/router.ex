defmodule Anoma.Node.Router do
  @moduledoc """
  I am the Router, the central networking component of Anoma. My
  functionality includes several services:

  1. I provide central routing infrastructure for inter-node and
     intra-node communication

  1. I provide service's for topic creation and
     `Anoma.Node.Router.Engine` creation

  1. I provide a behavior interface similar to `GenServer` for both
     [`Engine`](`Anoma.Node.Router.Engine`) messaging and `Topic`
     messaging


  ### Router Topology

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

  ### Server Terminology

  For the sake of this document, the word `Server` will refer to both
  [`Engines`](`Anoma.Node.Router.Engine`) and `Topics`.

  ### Router PIDs

  The IDs of the various topics and
  [engines](`Anoma.Node.Router.Engine`), are not regular Erlang
  PID's. Instead we use our own address schema laid out in
  `Anoma.Node.Router.Addr`. Further information can also be found in
  our specs (link to specs when they are online).

  ### Public API

  I provide the following public functionality:

  #### Router API

  I offer an API for anyone wishing to call me. These functions are:

  - `start/0`
  - `start/1`
  - `new_topic/1`
  - `subscribe_topic/2`
  - `start_engine/3`
  - `start_engine/4`
  - `subscribe_topic/3`

  #### Server APIs

  When writing server code, My module acts as a `GenServer` behavior,
  in that one should be using these functions when writing code to
  talk to the `Server`:

  - `call/2`
  - `cast/2`
  - `call/3`

  The API I offer is very reminiscent of `GenServer`'s API, however
  please read the [examples](#module-examples) section to get a sense
  on how the server code differs.

  #### Self-Cast/Call API

  I have separate API for assisting myself upon message reception from
  myself as a GenServer.

  - `handle_self_cast/3`
  - `handle_self_call/3`

  #### Other

  - `stop/0`
  - `self_addr/0`
  - `router/0`
  - `shutdown_node/1`
  - `dump_state/1`
  - `set_logger/2`
  - `send_raw/2`
  - `process_name/2`
  - `do_handle_external_cast/3`
  - `send_response/4`

  ### Client/Server APIs

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

  2. Even `handle_cast`'s take a `from` parameter

  3. We use `Anoma.Node.Router.Engine` and not `GenServer`

  4. Further `call/2` by default has an :infinite timeout

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

  alias __MODULE__
  alias Anoma.Crypto.Id
  alias Anoma.Node.Router.Addr
  alias Anoma.Node.{Transport, Logger, Transport2}

  @typedoc """
  I am the type of an Engine address.
  """

  @type addr() :: Addr.t()

  @typedoc """
  I am the type of Engine identification.

  ### Options

  - `{:id, Id.t()}` - Identify the engine by its full Anoma ID.
  - `{:id, Id.Extern.t()}` - Identify the engine by its external ID.
  - `{:supervisor, Supervisor.supervisor}` - Identify as supervisor.
  - `{:supervisor_mod, atom()}` - Identify as supervisor given a name.
  """

  @type engine_options() ::
          {:id, Id.t()}
          | {:id, Id.Extern.t()}
          | {:supervisor, Supervisor.supervisor()}
          | {:supvisor_mod, atom()}

  typedstruct do
    @typedoc """
    I am the type of the Anoma Router. I contain all necessary information
    for coordination of the Anoma network, including all registered local
    engines, topics, subscriptions, the message queues, message counter, as
    well as local identification information.

    ### Fields

    - `:local_engines` - The map of local engines external IDs to their full
                         IDs alongside their global process identifications.
    - `:topic_table` - The map of local topic external IDs to the set of their
                       subscribers.
                       Default: %{}
    - `:waiting_engines` - Map whose keys are external IDs of engines who make
                           a call and whose values are the external ID to whom
                           the message gets sent to alongside the message
                           counter.
                           Default : %{}
    - `:max_call_id` - The latest message counter.
                       Default: 0
    - `:local_engine_subs` - Similar to `:topic_table` but for local engine
                             addresses and the values are individual external
                             IDs.
                             Default: %{}
    - `:id` - Router external ID.
    - `:internal_id` - Full Router Anoma ID.
    - `:addr` - Router Address.
    - `:supervisor` - Supervisor name.
    - `:transport` - Name of the used Transport.
    - `:msg_queue` - Map of IDs to the list of pending messages.
                     Default: %{}
    - `:logger` - The Logger Engine address for Event keeping.
    """

    # slightly space-inefficient (duplicates extern), but more convenient
    field(:local_engines, %{Id.Extern.t() => {Id.t(), Process.dest()}})
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
    field(:local_engine_subs, %{Addr.t() => Id.Extern.t()}, default: %{})
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
  I am the minimal start function.

  I call `start/1` with a random `Anoma.Crypto.Id` and default state.
  """
  @spec start() :: :ignore | {:error, any()} | {:ok, Addr.t(), Addr.t()}
  def start() do
    start({Id.new_keypair(), Id.new_keypair(), %Router{}})
  end

  @doc """
  I am the Router start function.

  I start a new router using the specified Anoma ID for the Router and the
  transport. (Additionally, you can feed me a router state)

  Using the fed in external ID, I name the supervisor using `process_name/2`
  and start it as a Dynamical Supervisor with appropriate name.

  Similarly, I create the router name and address using the external ID and
  start the Router and Transport structures as children to the specified
  supervisor.

  On success, return {:ok, router_addr, transport_addr}
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
      server: router_name
    }

    transport_name = process_name(Transport, transport_id.external)

    transport_addr = %Addr{
      id: transport_id.external,
      server: transport_name
    }

    with {:ok, _} <-
           DynamicSupervisor.start_child(
             supervisor,
             {__MODULE__,
              {router_id, router_addr, transport_addr, router_state}}
           ),
         {:ok, connection_pool} <-
           DynamicSupervisor.start_child(supervisor, Transport2.Supervisor),
         {:ok, _} <-
           DynamicSupervisor.start_child(
             supervisor,
             {Anoma.Node.Router.Engine,
              {router_addr, Transport, transport_id,
               {router_addr, router_id, transport_id, connection_pool}}}
           )
           |> tap(fn x -> IO.inspect(x, label: "engine router") end) do
      IO.inspect({router_addr, router_id, transport_id, connection_pool})
      {:ok, router_addr, transport_addr}
    end
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  # not sure exactly how this will work for real, but it's convenient
  # to have for testing right now
  @doc """
  I am the topic-creation function.

  I create a new topic with a new random external key.
  """
  @spec new_topic(Addr.t()) :: {:ok, Addr.t()} | {:error, :already_exists}
  def new_topic(router) do
    call(router, {:create_topic, Id.new_keypair().external, :local})
  end

  @doc """
  I am the topic-creation function.

  I create a new topic with specified external ID from a full Anoma ID.
  """

  @spec new_topic(Addr.t(), Id.t()) ::
          {:ok, Addr.t()} | {:error, :already_exists}
  def new_topic(router, id) do
    call(router, {:create_topic, id.external, :local})
  end

  @doc """
  I am the function launching a new Anoma Engine instance

  I start the Engine using `Anoma.Node.Router.Engine` with the base settings
  from the ROuter. As a server, I start it as a new child to the router
  supervisor.

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

  @doc """
  I forcibly stop an engine by calling `terminate_child/2`.

  TODO, please try to be more friendly to the engine, try to send a
  `:shutdown` and waiting.

  ### Example

  > node = ENode.fresh_full_node(...)
  > worker_zero = wait_for_tx(node.mempool, {:kv, zero}, 5000).addr
  > Router.stop_engine(node.router, worker_zero)
  """
  @spec stop_engine(Addr.t(), Addr.t(), [engine_options()]) ::
          :ok | {:error, :not_found}

  def stop_engine(router, engine, options \\ []) do
    keys =
      Keyword.validate!(options,
        id: Id.new_keypair(),
        supervisor: call(router, :supervisor),
        supervisor_mod: DynamicSupervisor
      )

    supervisor = keys[:supervisor]
    pid = Addr.pid(engine)

    if pid do
      keys[:supervisor_mod].terminate_child(supervisor, pid)
    else
      {:error, :not_found}
    end
  end

  @doc """
  I do nothing.
  """

  @spec stop(addr()) :: nil
  def stop(_router) do
  end

  @doc """
  I am the logger-setting function.

  After the Logger Engine has been started, we can add it to the Router to
  log events using it. To avoid recursive calls, some logging utilizes only
  the internal Elixir logger.
  """

  @spec set_logger(addr(), addr()) :: :ok
  def set_logger(router, logger) do
    cast(router, {:set_logger, logger})
  end

  ############################################################
  #                     Public Server API                    #
  ############################################################

  @doc """
  I send a message directly using Kernel.send/1 to the local process, assuming
  there is one.
  """
  @spec send_raw(Addr.t(), term()) :: :ok
  def send_raw(addr, msg) do
    send(Addr.noncanonical_server(addr), msg)
    :ok
  end

  @doc """
  I am the Router cast function.

  I make a asynchronous cast to the `Server`.

  Call has a few interesting cases we can consider

  1. Casting to a local [`Engine`](`Anoma.Node.Router.Engine`)

  2. Casting to a non local [`Engine`](`Anoma.Node.Router.Engine`)

  3. Casting to a `Topic`


  For the local [`Engine`](`Anoma.Node.Router.Engine`), then their
  [`Engine.handle_cast/3`](`c:Anoma.Node.Router.Engine.handle_cast/3`)
  will be called on the [`Engine`](`Anoma.Node.Router.Engine`) to
  handle the request.

  Casting a non local [`Engine`](`Anoma.Node.Router.Engine`) isn't
  handled yet.

  For `Topics` any `cast/2` sent, then triggers a series of `cast/2`
  to all subscribed `Process`'s

  """
  @spec cast(Addr.t(), term()) :: :ok
  def cast(addr, msg) do
    case Addr.noncanonical_server(addr) do
      nil ->
        log_info({:casting, addr})

        # todo message serialisation should happen here (but there is some subtlety
        # because local topics also go through the router, and we don't want to
        # bother serialising anything then)
        GenServer.cast(Addr.server(router()), {:cast, addr, self_addr(), msg})

      server ->
        GenServer.cast(server, {:router_cast, self_addr(), msg})
    end
  end

  @doc """
  I am a call function.

  I use `call/3` with the specified address and message and set the default
  timeout o 5000.
  """
  @spec call(Addr.t(), term()) :: term()
  def call(addr, msg) do
    # default timeout for GenServer.call
    call(addr, msg, 5000)
  end

  @doc """
  I am the Router call function.

  I make a synchronous call to the `Server` and wait for a reply.

  Call has a few interesting cases we can consider

  1. Calling a local [`Engine`](`Anoma.Node.Router.Engine`)

  2. Calling a non local [`Engine`](`Anoma.Node.Router.Engine`)

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
  def call(addr, msg, timeout) do
    case Addr.noncanonical_server(addr) do
      nil ->
        call_nonlocal(addr, msg, timeout)

      server ->
        # use an infinite timeout for local calls--timeout is only
        # significant for inter-node calls
        GenServer.call(server, {:router_call, self_addr(), msg}, :infinity)
    end
  end

  defp call_nonlocal(addr, msg, timeout) do
    log_info({:calling, addr})
    # todo message serialisation should happen here
    GenServer.cast(
      Addr.server(router()),
      {:call, addr, self_addr(), msg, timeout}
    )

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
          {:error, error} -> {:error, {:message_encoding, error}}
        end
    end
  end

  @doc """
  I am a function sending responses.

  I send a response to an externally received call. This is a low-level
  routine, intended to be called only by the internals of Router and Engine.
  """
  @spec send_response(Addr.t(), Addr.t(), non_neg_integer(), binary()) :: :ok
  def send_response(router, dst, cookie, encoded_response) do
    Router.cast(router, {:send_response, dst, cookie, encoded_response})
  end

  @doc """
  I am a function for Node shutdown.

  As an Anoma Node is associated with a specific Router, I can cast a
  message to shut down all local Engines, the Router, and the Node
  itself using only the Router.

  We should tweak this in the future, with a mode to just shut down
  the Anoma Node itself.
  """

  @spec shutdown_node(addr()) :: :ok
  def shutdown_node(router) do
    Router.cast(router, :shutdown_everything)
  end

  @doc """
  I am the state cleanup function.

  I empty the entire state of the Router whose Address I receive.
  """

  @spec dump_state(addr()) :: Router.t()
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
      | local_engines: Map.delete(s.local_engines, Addr.id(addr)),
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
    case Anoma.Serialise.unpack(msg) do
      {:ok, msg} ->
        do_handle_external_cast(msg, src, s)

      {:error, error} ->
        log_info({:message_encoding, error, s.logger})
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

  def handle_info({:timeout_message, src_addr, call_id}, s) do
    {
      :noreply,
      # we have to check the call id matches; otherwise this could spuriously
      # fail.  there are some other potential ways of dealing with timeouts,
      # but meh
      case Map.get(s.waiting_engines, Addr.id(src_addr)) do
        {^call_id, _} ->
          send_raw(src_addr, {:router_call_response, {:error, :timed_out}})

          %{
            s
            | waiting_engines:
                Map.delete(s.waiting_engines, Addr.id(src_addr))
          }

        _ ->
          s
      end
    }
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  @doc """
  I am a handle_self_cast function.

  Once `cast/2` or `do_cast` gets done finding the appropriate server to
  send the message to, some of them get forwarded back to the Router
  through itself. I am the helper function helping to process such
  interactions.

  ### Pattern-Matching Variations

  - `handle_self_cast{:set_logger, logger}, src, s` -
      Assigns the specified Logger Engine address to the Router.
  - `handle_self_cast(:shutdown_everything, _, _)` -
      Calls `System.stop` to shutdown the entire system.
  - `handle_self_cast({:send_response, _, _, _}, _, _)` -
      Sends the response message to transport with specified cookie.
  - `handle_self_cast({:p2p_raw, _, _}, _, _)` -
      Handles p2p messaging, either sending raw messages or post-processing
      them using Router functionality. Note that unknown message formats get
      dropped. The droppages get logged.
  """

  @spec handle_self_cast(
          :shutdown_everything
          | {:set_logger, addr()}
          | {:p2p_raw, any(), any()}
          | {:send_response, addr(), any(), any()},
          any(),
          Router.t()
        ) :: Router.t() | :ok
  def handle_self_cast({:set_logger, logger}, _src, s) do
    %Router{s | logger: logger}
  end

  def handle_self_cast(:shutdown_everything, _src, s) do
    log_info({:shutdown, s.logger})
    System.stop()
  end

  def handle_self_cast({:send_response, dst, cookie, response}, src, s) do
    send_to_transport(s, Addr.id(dst), Addr.id(src), [
      :response,
      cookie,
      response
    ])

    s
  end

  def handle_self_cast({:p2p_raw, _node_id, msg}, %{server: server}, s)
      when server != nil do
    logger = s.logger

    case msg do
      %{"data" => data, "sig" => sig} ->
        case Anoma.Serialise.unpack(data) do
          {:ok, [dst_id = %Id.Extern{}, src_id = %Id.Extern{}, msg]} ->
            if not Anoma.Crypto.Sign.verify_detached(sig, data, src_id.sign) do
              log_info({:drop_sig, logger})
              s
            else
              src_addr = id_to_addr(s, src_id)
              dst_addr = id_to_addr(s, dst_id)

              if not Map.has_key?(s.local_engines, dst_id) do
                log_info({:drop_eng, logger})
                s
              else
                case msg do
                  [:response, call_id, msg] ->
                    # is the engine waiting for a message with this id from this id (todo maybe signs for)?
                    if Map.get(s.waiting_engines, dst_id) ===
                         {call_id, src_id} do
                      send_raw(dst_addr, {:router_call_response, msg})

                      %{
                        s
                        | waiting_engines:
                            Map.delete(s.waiting_engines, dst_id)
                      }
                    else
                      # timeout expired, or confused or malicious correspondant; drop
                      log_info({:drop_call, logger})

                      s
                    end

                  [:cast, msg] ->
                    GenServer.cast(
                      Addr.server(dst_addr),
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
                        Addr.server(dst_addr),
                        {:router_external_call, src_addr, call_id, msg}
                      )
                    end

                    s

                  # unknown message format; drop
                  _ ->
                    log_info({:drop_form_3, logger, msg})

                    s
                end
              end
            end

          {:error, error} ->
            log_info({:drop_form_2, logger, error})

            s
        end

      _ ->
        log_info({:drop_form_1, logger, msg})
        s
    end
  end

  @doc """
  I am a handle_self_call function.

  Once `call/2` or `do_call` gets done finding the appropriate server to
  send the message to, some of them get forwarded back to the Router
  through itself. I am the helper function helping to process such
  interactions.

  ### Pattern-Matching Variations

  - `handle_self_call(:supervisor, src, s)` -
      Asks for a supervisor of the router.
  - `handle_self_call({:create_topic, id, :local}, _, _)` -
      Unless the topic has already been added to the topic table, puts it
      to the table with an empty set of subscribers and return the new
      structure with specified ID.
  - `handle_self_call({:subscribe_topic, topic, :local}, _, _)` -
      Updates the topic tables and engine subscriber fields in the Router to
      include the new specified subscriber.
  - `handle_self_call({:unsubscribe_topic, topic, :local}, _, _)` -
      Updates the topic tables and engine subscriber fields in the Router to
      remove the specified subscriber information.
  """

  @spec(
    handle_self_call(
      :supervisor
      | {:create_topic, Id.Extern.t(), :local}
      | {:subscribe_topic, Id.Extern.t(), :local}
      | {:unsubscribe_topic, Id.Extern.t(), :local},
      addr(),
      Router.t()
    ) ::
      {atom, Router.t()}
      | {:ok, Router.t()}
      | {{:error, :no_such_topic}, Router.t()}
      | {{:error, :already_exists}, Router.t()},
    {{:ok, Id.Extern.t()}, Router.t()}
  )
  def handle_self_call(:supervisor, _, s) do
    {s.supervisor, s}
  end

  # create topic.  todo non local topics.  todo the topic should get
  # its own engine so distinct topics can be dap
  def handle_self_call({:create_topic, id, :local}, _, s) do
    if Map.has_key?(s.topic_table, id) do
      {{:error, :already_exists}, s}
    else
      {{:ok, id},
       %{s | topic_table: Map.put(s.topic_table, id, MapSet.new())}}
    end
  end

  def handle_self_call({:subscribe_topic, topic, :local}, addr, s) do
    topic = Addr.id!(topic)

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
  def handle_self_call({:unsubscribe_topic, topic, :local}, addr, s) do
    topic = Addr.id!(topic)

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

  @spec do_cast(t(), Addr.t(), Addr.t(), term()) :: t()
  defp do_cast(s, dst, src, msg) do
    cond do
      # send directly to a local engine
      Addr.server(dst) ->
        log_info({:dir_cast, dst})
        GenServer.cast(Addr.server(dst), {:router_cast, src, msg})
        s

      # send to a topic we know about
      is_map_key(s.topic_table, Addr.id!(dst)) ->
        Enum.reduce(Map.get(s.topic_table, Addr.id!(dst)), s, fn recipient,
                                                                 s ->
          do_cast(s, recipient, src, msg)
        end)

      # send to remote address
      true ->
        # todo: serialisation of msg should happen elsewhere
        # also if there's no id we could return the error to the caller (or forge an id...)
        send_to_transport(s, Addr.id!(dst), Addr.id!(src), [
          :cast,
          Anoma.Serialise.pack(msg)
        ])

        s
    end
  end

  @spec do_call(t(), Addr.t(), Addr.t(), term(), :erlang.timeout()) :: t()
  defp do_call(s, dst, src, msg, timeout) do
    case Addr.server(dst) do
      nil ->
        call_remote(s, Addr.id!(dst), src, msg, timeout)

      server ->
        # call to a local engine
        res = GenServer.call(server, {:router_call, src, msg}, :infinity)
        send_raw(src, {:router_call_response, res})
        s
    end
  end

  @spec call_remote(Router.t(), Id.Extern.t(), addr(), any, :erlang.timeout()) ::
          t()
  # call to remote address
  defp call_remote(s, dst_id, src_addr, msg, timeout) do
    src_id = Addr.id(src_addr)
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

  @doc """
  I am a function handling non-local casts to the Router.

  Currently I only process a message to shut everything down by calling
  `System.stop/0`
  """

  @spec do_handle_external_cast(:shutdown_everything, any(), t()) :: :ok
  def do_handle_external_cast(:shutdown_everything, _src, s) do
    log_info({:shutdown, s.logger})
    System.stop()
  end

  @spec send_to_transport(t(), Id.Extern.t(), any(), any()) :: :ok
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
          Process.dest(),
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
  @spec atom_to_nice_string(atom()) :: binary()
  defp atom_to_nice_string(atom) do
    res = Atom.to_string(atom)

    if String.starts_with?(res, "Elixir.") do
      binary_part(res, 7, byte_size(res) - 7)
    else
      res
    end
  end

  @spec id_to_addr(t(), Id.Extern.t()) :: addr()
  defp id_to_addr(s, id) do
    %{
      s.addr
      | id: id,
        server: elem(Map.get(s.local_engines, id, {nil, nil}), 1)
    }
  end

  @doc """
  I am a function formulating a process name.

  Given a module name and an external ID, I append to the module name the
  encoded sign field of the ID and return it as an atom.
  """

  @spec process_name(atom(), Id.Extern.t()) :: atom()
  def process_name(module, id) do
    :erlang.binary_to_atom(
      atom_to_nice_string(module) <> " " <> Base.encode64(id.sign)
    )
  end

  @doc """
  I am a function providing the default address to the process which runs
  me.

  My server is either the specified engine server or my PID.
  """

  @spec self_addr() :: Addr.t()
  def self_addr() do
    %Addr{
      id: Process.get(:engine_id),
      server: Process.get(:engine_server) || self()
    }
  end

  @doc """
  I am a function to search for the Engine Router inside an Engine.
  """

  @spec router() :: Addr.t()
  def router() do
    Process.get(:engine_router) || exit("no known router")
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

  defp log_info({:casting, addr}) do
    Logger.add(nil, :info, "Casting to non-local addr #{inspect(addr)}")
  end

  defp log_info({:calling, addr}) do
    Logger.add(nil, :info, "Calling non-local addr #{inspect(addr)}")
  end

  defp log_info({:message_encoding, error, logger}) do
    Logger.add(
      logger,
      :debug,
      "Not able to decode message: #{inspect(error)}"
    )
  end

  defp log_info({:shutdown, logger}) do
    Logger.add(logger, :info, "Shutting down the system")
  end

  defp log_info({:drop_sig, logger}) do
    Logger.add(logger, :debug, "Dropping message; bad signature")
  end

  defp log_info({:drop_eng, logger}) do
    Logger.add(logger, :debug, "Dropping message; unknown engine")
  end

  defp log_info({:drop_call, logger}) do
    Logger.add(
      logger,
      :debug,
      "Dropping message; response but no corresponding call"
    )
  end

  defp log_info({:drop_form_1, logger, msg}) do
    Logger.add(
      logger,
      :debug,
      "Dropping message (1); invalid format #{inspect(msg)}"
    )
  end

  defp log_info({:drop_form_2, logger, data}) do
    Logger.add(
      logger,
      :debug,
      "Dropping message (2); invalid format #{inspect(data)}"
    )
  end

  defp log_info({:drop_form_3, logger, msg}) do
    Logger.add(
      logger,
      :debug,
      "Dropping message (3); invalid format #{inspect(msg)}"
    )
  end

  defp log_info({:dir_cast, dst}) do
    Logger.add(nil, :info, "Cast to #{inspect(dst)}")
  end
end
