defmodule Anoma.Node.Transport2.Router do
  @moduledoc """
  I am the router module. I route messages to local and remote engines.

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
  alias Anoma.Node.Transport2.EngineProxy
  alias Anoma.Node.Transport2.TCPClient
  alias Anoma.Node.Transport2.TCPServer
  alias Anoma.Node.Transport2.TCPSupervisor

  import Anoma.Node.Transport2.Addressing

  ############################################################
  #                    State                                 #
  ############################################################

  @typedoc """
  I am the type of the configuration to start a router process.
  """
  @type router_config :: map()

  typedstruct do
    @typedoc """
    I am the state of the router.

    My fields contain information to communicate with other engines.

    ### Fields
     - `:config` - The configuration parameters of the node.
    """
    field(:config, router_config())
  end

  ############################################################
  #                    Genserver Helpers                     #
  ############################################################

  def start_link([config]) do
    GenServer.start_link(__MODULE__, config, name: __MODULE__)
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @doc """
  I start a new TCP server process.
  """
  @spec start_tcp_server() :: :ok
  def start_tcp_server() do
    GenServer.call(__MODULE__, {:start_tcp_server})
  end

  @spec start_proxy_engine(any()) :: :ok
  @doc """
  I start a new TCP client to connect to a remote node at the given host and port.
  """
  @spec start_tcp_client(
          :inet.socket_address() | :inet.hostname(),
          :inet.port_number()
        ) :: :ok
  def start_tcp_client(host, port) do
    GenServer.call(__MODULE__, {:start_tcp_client, host, port})
  end

  @doc """
  Given the id of an engine, I start a proxy engine for this id.
  This proxy can be used to route messages to a remote node.
  """
  @spec start_proxy_engine(Id.t()) :: :ok
  def start_proxy_engine(engine_id) do
    GenServer.cast(__MODULE__, {:start_proxy_engine, engine_id})
  end

  @doc """
  Given the id of a node and engine type, I lookup the address of the engine's proxy.
  """
  def lookup_engine(engine_id, engine_type) do
    GenServer.call(__MODULE__, {:lookup_engine, engine_id, engine_type})
  end

  @doc """
  I send an async message to an engine by sending it to its proxy.
  The proxy will then forward it to via a connection (e.g., TCP).
  """
  @spec async_send(Id.t(), atom(), any()) :: :ok
  def async_send(engine_id, engine_type, message) do
    GenServer.cast(
      get_address_for(engine_id, engine_type),
      {:send_async, message}
    )
  end

  # @doc """
  # I send a synchronous message to an engine by sending it to its proxy
  # and waiting for a reply.
  # **NOTE**: this function just sends an async now.
  # """
  @spec sync_send(Id.t(), atom(), any()) :: term()
  def sync_send(engine_id, engine_type, message, timeout \\ 0) do
    GenServer.call(
      get_address_for(engine_id, engine_type),
      {:send_sync, message, timeout},
      timeout
    )
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  # @doc """
  # I initialize a new router with the given configuration.
  # """
  @impl true
  @spec init(router_config()) :: {:ok, Router.t()}
  def init(config) do
    state = %__MODULE__{config: config}
    {:ok, state}
  end

  @impl true

  # @doc """
  # I start a new proxy engine.
  # """
  def handle_cast({:start_proxy_engine, remote_router_id}, state) do
    {:ok, state} = handle_start_proxy_engine(remote_router_id, state)
    {:noreply, state}
  end

  @impl true
  # @doc """
  # I start a new TCP server.
  # """
  def handle_call({:start_tcp_server}, _from, state) do
    case handle_start_tcp_server(state) do
      {:ok, state} ->
        {:reply, :ok, state}

      {:error, reason, state} ->
        {:reply, {:error, reason}, state}
    end
  end

  # @doc """
  # I start a new TCP client.
  # """
  def handle_call({:start_tcp_client, host, port}, _from, state) do
    {:reply, handle_start_tcp_client(host, port, state), state}
  end

  # @doc """
  # I lookup the pid of a proxy engine for a given id and type.
  # """
  def handle_call({:lookup_engine, id, type}, _from, state) do
    {:reply, handle_lookup_engine(id, type), state}
  end

  ############################################################
  #               Genserver Implementation                   #
  ############################################################

  @doc """
  I start a new TCP server process.

  I use the configuration of the router to start the server on the proper port and host.
  """
  @spec handle_start_tcp_server(t()) ::
          {:ok, t()} | {:error, :failed_to_start_tcp_server, t()}
  def handle_start_tcp_server(state) do
    %{config: %{network: network, router_key: router_key}} = state

    args = [
      host: network.host,
      port: network.tcp_port,
      router_key: router_key
    ]

    case DynamicSupervisor.start_child(TCPSupervisor, {TCPServer, args}) do
      {:ok, _} ->
        {:ok, state}

      {:ok, _, _} ->
        {:ok, state}

      err ->
        IO.puts("an error occurred starting the tcp server: #{inspect(err)}")
        {:error, :failed_to_start_tcp_server, state}
    end
  end

  @doc """
  I start a new TCP client process that will connect to a remote TCP server.
  """
  @spec handle_start_tcp_client(
          :inet.socket_address() | :inet.hostname(),
          :inet.port_number(),
          t()
        ) :: {:ok, t()}
  def handle_start_tcp_client(host, port, state) do
    config = state.config

    args = [
      host: host,
      port: port,
      router_key: config.router_key
    ]

    case DynamicSupervisor.start_child(TCPSupervisor, {TCPClient, args}) do
      {:ok, _} ->
        {:ok, state}

      {:ok, _, _} ->
        {:ok, state}

      err ->
        IO.puts("an error occurred starting the tcp server: #{inspect(err)}")
        {:error, :failed_to_start_tcp_client, state}
    end
  end

  @doc """
  I start a new proxy engine for a given router id.
  """
  @spec handle_start_proxy_engine(Id.t(), t()) :: {:ok, t()}
  def handle_start_proxy_engine(id, state) do
    res =
      DynamicSupervisor.start_child(
        TCPSupervisor,
        {EngineProxy, [id]}
      )

    case res do
      {:ok, _} ->
        {:ok, state}

      {:ok, _, _} ->
        {:ok, state}

      err ->
        IO.puts(
          "an error occurred starting the proxy engine: #{inspect(err)}"
        )

        {:ok, state}
    end
  end

  @doc """
  I lookup the pid of a proxy engine for a given id and type.

  ## Examples

    iex> Router.lookup_engine(some_id, :router)
    {:ok, pid}

    iex> Router.lookup_engine(some_id, :router)
    {:error, :not_found}
  """
  @spec handle_lookup_engine(Id.t(), atom()) ::
          {:error, :not_found} | {:ok, pid()}
  def handle_lookup_engine(id, engine_type) do
    case Registry.lookup(ProxyRegister, %{
           remote_id: id,
           type: engine_type
         }) do
      [{pid, _meta}] ->
        {:ok, pid}

      [] ->
        {:error, :not_found}
    end
  end

  ############################################################
  #                           Helpers                        #
  ############################################################

  @doc """
  I am a helper function to dump the entire register.
  I am used for debugging purposes.
  """
  def dump_register() do
    Registry.select(ProxyRegister, [
      {{:"$1", :"$2", :"$3"}, [], [{{:"$1", :"$2", :"$3"}}]}
    ])
    |> Enum.sort()
  end
end
