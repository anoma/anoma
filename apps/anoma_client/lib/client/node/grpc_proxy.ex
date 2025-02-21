defmodule Anoma.Client.Node.GRPCProxy do
  use GenServer
  use TypedStruct

  alias Anoma.Client.Node.RPC

  require Logger

  @args [
    :node_id,
    :host,
    :port,
    :grpc_port,
    :client_id
  ]

  ############################################################
  #                       Types                              #
  ############################################################

  @typep startup_options() :: [
           {:node_id, String.t()},
           {:host, String.t()},
           {:port, integer()},
           {:grpc_port, integer()},
           {:client_id, String.t()}
         ]

  ############################################################
  #                    State                                 #
  ############################################################

  typedstruct do
    @typedoc """
    I am the state of a TCP listener.

    My fields contain information to listen for TCP connection with a remote node.

    ### Fields
    - `:port`      - The port on which the remote node is listening to GRPC.
    - `:host`      - The host on which the remote node is listening to GRPC.
    - `:channel`   - The channel to the remote grpc.
    - `:node_id`   - The id of the remote node.
    - `:grpc_port` - The grpc port of the client.
    - `:client_id` - The id of the client.
    """
    field(:port, integer())
    field(:host, String.t())
    field(:channel, any())
    field(:node_id, String.t())
    field(:grpc_port, integer())
    field(:client_id, String.t())
  end

  ############################################################
  #                    Genserver Helpers                     #
  ############################################################

  @spec start_link(startup_options) :: GenServer.on_start()
  def start_link(args) do
    args = Keyword.validate!(args, @args)
    GenServer.start_link(__MODULE__, args, name: __MODULE__)
  end

  @impl true
  def init(args) do
    Logger.debug("grpc proxy started #{inspect(args)}")
    state = struct(__MODULE__, Enum.into(args, %{}))

    case GRPC.Stub.connect("#{state.host}:#{state.port}") do
      {:ok, channel} ->
        {:ok, %{state | channel: channel}, {:continue, :advertise}}

      _err ->
        {:stop, :node_unreachable}
    end
  end

  @impl true
  # advertise to the node I just connected to
  def handle_continue(:advertise, state) do
    RPC.advertise(
      state.channel,
      state.node_id,
      state.client_id,
      state.grpc_port,
      "localhost"
    )

    {:noreply, state}
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @spec list_intents() ::
          {:ok, [binary()]}
          | {:error, :failed_to_fetch_intents}
  def list_intents() do
    GenServer.call(__MODULE__, :list_intents)
  end

  @spec add_intent(binary()) ::
          {:ok, :added} | {:error, :add_intent_failed, String.t()}
  def add_intent(intent) do
    GenServer.call(__MODULE__, {:add_intent, intent})
  end

  @spec add_transaction(binary()) :: any()
  def add_transaction(jammed_nock) do
    GenServer.call(__MODULE__, {:add_transaction, jammed_nock})
  end

  @spec subscribe(String.t()) ::
          {:ok, :subscribed} | {:error, :subscribe_failed, any()}
  def subscribe(topic) do
    GenServer.call(__MODULE__, {:subscribe, topic})
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  @impl true
  def handle_call(:list_intents, _from, state) do
    result = RPC.list_intents(state.channel, state.node_id)

    {:reply, result, state}
  end

  def handle_call({:add_intent, intent}, _from, state) do
    result = RPC.add_intent(state.channel, state.node_id, intent)
    {:reply, result, state}
  end

  def handle_call({:add_transaction, transaction}, _from, state) do
    result = RPC.add_transaction(state.channel, state.node_id, transaction)

    {:reply, result, state}
  end

  def handle_call({:subscribe, topic}, _from, state) do
    result =
      RPC.subscribe(state.channel, state.node_id, state.client_id, topic)

    {:reply, result, state}
  end

  @impl true
  # the connection to the remote node was closed.
  def handle_info({:gun_down, _pid, :http2, :closed, []}, state) do
    {:stop, :disonnect, state}
  end
end
