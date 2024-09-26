defmodule Anoma.Node.Transport.Router do
  @moduledoc """
  I am the router module. I route messages and start connections to other nodes.

  ### Public API

  I provide the following public functionality:

  - `start_tcp_server/0`
  - `start_tcp_client/0`
  """
  use GenServer
  use TypedStruct

  require Logger

  alias __MODULE__
  alias Anoma.Crypto.Id
  alias Anoma.Node.Transport.EngineProxy
  alias Anoma.Node.Transport.Registry
  alias Anoma.Node.Transport.TCP

  @typedoc """
  Shorthand type for socket.
  """
  @type hostname :: :inet.socket_address() | :inet.hostname()

  @typedoc """
  Shorthand type for port number.
  """
  @type port_number :: :inet.port_number()

  @typedoc """
  I am the type of the configuration to start a router process.
  """
  @type router_config :: map()

  ############################################################
  #                    State                                 #
  ############################################################

  typedstruct do
    @typedoc """
    I am the state of the router.

    My fields contain information to communicate with other engines.

    ### Fields
    - `:node_id` - The id of the node to which this router belongs.
    - `:port`    - The port on which this router listens for incoming connections.
    """
    field(:node_id, Id.t())
    field(:port, port_number())
  end

  ############################################################
  #                    Genserver Helpers                     #
  ############################################################

  def start_link(args) do
    args = Keyword.validate!(args, [:node_id])

    # register as a router for the local node, at the local registry.
    node_id = args[:node_id]
    name = Registry.key(node_id, node_id, __MODULE__)

    GenServer.start_link(__MODULE__, args, name: name)
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  @doc """
  I initialize a new router with the given configuration.

  ### Options

  - `:node_id` - The key of the local node.
  """
  @impl true
  @spec init(Keyword.t()) :: {:ok, Router.t()}
  def init(args) do
    Logger.debug("starting router with #{inspect(args)}")

    args = Keyword.validate!(args, [:node_id])

    state = struct(__MODULE__, Enum.into(args, %{}))

    {:ok, state}
  end

  # ----------------------------------------------------------------------------
  # Calls

  @impl true
  # @doc """
  # I handle the call to start a new TCP server.
  # """
  def handle_call({:start_tcp_server, host, port}, _from, state) do
    Logger.debug("starting tcp server")
    {:ok, _pid, port} = handle_start_tcp_server(host, port, state.node_id)
    {:reply, {:ok, port}, %{state | port: port}}
  end

  # @doc """
  # I handle the call to start a new TCP client.
  # """
  def handle_call({:start_tcp_client, host, port}, _from, state) do
    Logger.debug("starting tcp client")
    {:ok, _pid} = handle_start_tcp_client(host, port, state.node_id)
    {:reply, :ok, state}
  end

  # @doc """
  # I handle the call to start a new proxy engine.
  # """
  def handle_call({:start_proxy, remote_node_id, type}, _, state) do
    Logger.debug("starting proxy engine")

    {:ok, _pid} =
      handle_start_proxy_engine(remote_node_id, type, state.node_id)

    {:reply, :ok, state}
  end

  # @doc """
  # I handle the call handling a synchronous message addressed to me.
  # """
  def handle_call({:synchronous_message, message, _timeout}, _from, state) do
    label = :erlang.phash2(state.node_id)
    message = inspect(message)
    Logger.debug("router received sync message: #{label} #{message}")

    {:reply, :ok, state}
  end

  # ----------------------------------------------------------------------------
  # Casts

  @impl true
  # @doc """
  # I handle the call handling an asynchronous message addressed to me.
  # """
  def handle_cast({:asynchronous_message, message}, state) do
    label = :erlang.phash2(state.node_id)
    message = inspect(message)
    IO.puts("router received async message: #{label} #{message}")

    {:noreply, state}
  end

  ############################################################
  #               Static Public RPC API                      #
  ############################################################

  @doc """
  I ask the local router to send an asynchronous message to another router.
  The local router will lookup the proxy for the router and send the message.

  ### Options

    - `:node_id` - The id of the local node.
  """
  @spec send_async(Id.t(), atom(), any(), Keyword.t()) :: :ok
  def send_async(remote_node_id, type, message, opts \\ []) do
    opts = Keyword.validate!(opts, [:node_id])
    node_id = Keyword.get(opts, :node_id, nil)

    # I should either find the local router, or a proxy engine for the remote router
    case Registry.lookup(node_id, remote_node_id, type) do
      [{_node_id, ^type, _labels, pid, _}] ->
        GenServer.cast(pid, {:asynchronous_message, message})

      [] ->
        :ok
    end
  end

  @doc """
  I ask the local router to send an asynchronous message to another router.
  The local router will lookup the proxy for the router and send the message.

  If the recipient is unknown to me, I raise an error.

  ### Options

    - `:node_id` - The id of the local node.
    - `:timeout` - The time to wait for a response.

  """
  @spec send_sync(Id.t(), atom(), any(), Keyword.t()) :: term()
  def send_sync(remote_node_id, type, message, opts \\ [timeout: 5000]) do
    opts = Keyword.validate!(opts, [:node_id, :timeout])
    node_id = Keyword.get(opts, :node_id, nil)
    timeout = Keyword.get(opts, :timeout, 5000)

    # I should either find the local router, or a proxy engine for the remote router
    case Registry.lookup(node_id, remote_node_id, type) do
      [{_node_id, ^type, _labels, pid, _}] ->
        expiration = System.monotonic_time(:millisecond) + timeout

        GenServer.call(
          pid,
          {:synchronous_message, message, expiration},
          timeout
        )

      [] ->
        raise "no process associated with #{inspect(remote_node_id)} and name #{inspect(type)}"
    end
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @doc """
  I start a new TCP server process.

  ### Options

    - `:node_id` - The id of the local node.
  """
  @spec start_tcp_server(hostname, port_number, Keyword.t()) ::
          {:ok, port_number()}
  def start_tcp_server(host, port, opts \\ []) do
    opts = Keyword.validate!(opts, [:node_id])
    node_id = Keyword.get(opts, :node_id, nil)
    router = Registry.key(node_id, node_id, __MODULE__)

    GenServer.call(router, {:start_tcp_server, host, port})
  end

  @doc """
  I start a new TCP client process.

  ### Options

    - `:node_id` - The id of the local node.
  """
  @spec start_tcp_client(hostname, port_number, Keyword.t()) :: :ok
  def start_tcp_client(host, port, opts \\ []) do
    opts = Keyword.validate!(opts, [:node_id])
    node_id = Keyword.get(opts, :node_id, nil)
    router = Registry.key(node_id, node_id, __MODULE__)

    GenServer.call(router, {:start_tcp_client, host, port})
  end

  @doc """
  I start a new proxy engine for a remote node.

  ### Options

    - `:node_id` - The id of the local node.
    - `:type`    - The type of the engine to start.
  """
  @spec start_proxy_engine(Id.t(), Keyword.t()) :: :ok
  def start_proxy_engine(remote_node_id, opts \\ []) do
    opts = Keyword.validate!(opts, [:node_id, :type])
    node_id = Keyword.get(opts, :node_id, nil)
    type = Keyword.get(opts, :type, nil)
    router = Registry.key(node_id, node_id, __MODULE__)

    GenServer.call(router, {:start_proxy, remote_node_id, type})
  end

  ############################################################
  #               Genserver Implementation                   #
  ############################################################

  @doc """
  I start a new TCP listener under the supervisor for my tcp connections.
  """
  @spec handle_start_tcp_server(any(), any(), any()) ::
          {:error, :failed_to_start_tcp_server}
          | {:ok, pid(), char()}
  def handle_start_tcp_server(host, port, local_node_id) do
    # arg list for the tcp listener process
    args = [host: host, port: port, node_id: local_node_id]

    # name of the supervisor under which the listener will be started
    supervisor = Registry.key(local_node_id, local_node_id, :tcp_supervisor)

    case DynamicSupervisor.start_child(supervisor, {TCP.Listener, args}) do
      {:ok, pid} ->
        port = TCP.Listener.port(pid)
        {:ok, pid, port}

      {:ok, pid, _} ->
        port = TCP.Listener.port(pid)
        {:ok, pid, port}

      err ->
        IO.puts("an error occurred starting the tcp server: #{inspect(err)}")
        {:error, :failed_to_start_tcp_server}
    end
  end

  @doc """
  I start a new TCP client process that will connect to a remote TCP server.
  """
  @spec handle_start_tcp_client(hostname, port_number, Id.t()) ::
          {:error, :failed_to_start_tcp_client}
          | {:ok, pid()}
  def handle_start_tcp_client(host, port, local_node_id) do
    args = [host: host, port: port, node_id: local_node_id]

    # name of the supervisor under which the client will be started
    supervisor = Registry.key(local_node_id, local_node_id, :tcp_supervisor)

    case DynamicSupervisor.start_child(supervisor, {TCP.Client, args}) do
      {:ok, pid} ->
        {:ok, pid}

      {:ok, pid, _} ->
        {:ok, pid}

      err ->
        IO.puts("an error occurred starting the tcp client: #{inspect(err)}")
        {:error, :failed_to_start_tcp_client}
    end
  end

  @doc """
  I start a new TCP client process that will connect to a remote TCP server.
  When the remote node already has an engine proxy, I return that pid.
  """
  @spec handle_start_tcp_client(Id.t(), atom(), Id.t()) ::
          {:error, :failed_to_start_engine_proxy}
          | {:ok, pid()}
  def handle_start_proxy_engine(remote_node_id, type, local_node_id) do
    args = [
      node_id: local_node_id,
      remote_node_id: remote_node_id,
      type: type
    ]

    # name of the supervisor under which the client will be started
    supervisor =
      Registry.key(local_node_id, local_node_id, :proxy_engine_supervisor)

    case DynamicSupervisor.start_child(supervisor, {EngineProxy, args}) do
      {:ok, pid} ->
        {:ok, pid}

      {:ok, pid, _} ->
        {:ok, pid}

      {:error, {:already_started, pid}} ->
        {:ok, pid}

      err ->
        IO.puts(
          "an error occurred starting the engine proxy: #{inspect(err)}"
        )

        {:error, :failed_to_start_engine_proxy}
    end
  end
end
