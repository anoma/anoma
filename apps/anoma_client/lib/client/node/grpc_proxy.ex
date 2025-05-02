defmodule Anoma.Client.Node.GRPCProxy do
  @moduledoc """
  I am the GRPC proxy. I am responsible to translate requests to a node into GRPC requests.
  I return the value from the requests, or return an error.

  """
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
          {:ok, [binary()]} | {:error, :failed_to_fetch_intents}
  def list_intents() do
    GenServer.call(__MODULE__, :list_intents)
  end

  @spec add_intent(binary()) ::
          {:ok, :added} | {:error, :add_intent_failed, String.t()}
  def add_intent(intent) do
    GenServer.call(__MODULE__, {:add_intent, intent})
  end

  @spec add_transaction(binary(), atom()) :: any()
  def add_transaction(jammed_nock, transaction_type) do
    GenServer.call(
      __MODULE__,
      {:add_transaction, jammed_nock, transaction_type}
    )
  end

  @spec subscribe(String.t()) ::
          {:ok, :subscribed} | {:error, :subscribe_failed, any()}
  def subscribe(topic) do
    GenServer.call(__MODULE__, {:subscribe, topic})
  end

  @spec add_read_only_transaction(binary()) ::
          {:ok, Noun.t()}
          | {:error, :absent}
          | {:error, :add_read_only_transaction_failed, String.t()}
  def add_read_only_transaction(jammed_nock) do
    GenServer.call(__MODULE__, {:add_ro_transaction, jammed_nock})
  end

  ############################################################
  #                      Testnet Public RPC API              #
  ############################################################

  @spec list_nullifiers() ::
          {:ok, [binary()]} | {:error, :failed_to_list_nullifiers, String.t()}
  def list_nullifiers() do
    GenServer.call(__MODULE__, {:list_nullifiers})
  end

  @spec list_unrevealed_commits() ::
          {:ok, [binary()]}
          | {:error, :failed_to_list_unrevealed_commits, String.t()}
  def list_unrevealed_commits() do
    GenServer.call(__MODULE__, {:list_unrevealed_commits})
  end

  @spec list_commits() ::
          {:ok, [binary()]}
          | {:error, :failed_to_list_commits, String.t()}
  def list_commits() do
    GenServer.call(__MODULE__, {:list_commits})
  end

  @spec list_unspent_resources() ::
          {:ok, [binary()]}
          | {:error, :failed_to_list_unspent_resources, String.t()}
  def list_unspent_resources() do
    GenServer.call(__MODULE__, {:list_unspent_resources})
  end

  @spec get_blocks({:before | :after, non_neg_integer()}) ::
          {:ok, [RPC.block()]} | {:error, :failed_to_get_blocks, String.t()}
  def get_blocks({direction, offset}) do
    GenServer.call(__MODULE__, {:get_blocks, direction, offset})
  end

  @spec get_latest_block() ::
          {:ok, RPC.block() | nil}
          | {:error, :failed_to_get_block, String.t()}
  def get_latest_block() do
    GenServer.call(__MODULE__, :get_latest_block)
  end

  @spec root :: {:ok, binary()} | {:error, :failed_to_get_root, String.t()}
  def root() do
    GenServer.call(__MODULE__, :get_root)
  end

  @spec filter([{:owner | :kind, binary()}]) ::
          {:ok, [binary()]}
          | {:error, :failed_to_filter_resources, String.t()}
  def filter(filters) do
    GenServer.call(__MODULE__, {:filter, filters})
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

  def handle_call(
        {:add_transaction, transaction, transaction_type},
        _from,
        state
      ) do
    result =
      RPC.add_transaction(
        state.channel,
        state.node_id,
        transaction,
        transaction_type
      )

    {:reply, result, state}
  end

  def handle_call({:subscribe, topic}, _from, state) do
    result =
      RPC.subscribe(state.channel, state.node_id, state.client_id, topic)

    {:reply, result, state}
  end

  def handle_call({:add_ro_transaction, transaction}, _from, state) do
    result =
      RPC.add_read_only_transaction(state.channel, state.node_id, transaction)

    {:reply, result, state}
  end

  # ----------------------------------------------------------------------------
  # Testnet

  def handle_call({:list_nullifiers}, _from, state) do
    nullifiers = RPC.list_nullifiers(state.channel, state.node_id)
    {:reply, nullifiers, state}
  end

  def handle_call({:list_unrevealed_commits}, _from, state) do
    commits = RPC.list_unrevealed_commits(state.channel, state.node_id)
    {:reply, commits, state}
  end

  def handle_call({:list_commits}, _from, state) do
    commits = RPC.list_commits(state.channel, state.node_id)
    {:reply, commits, state}
  end

  def handle_call({:list_unspent_resources}, _from, state) do
    unspent_resources =
      RPC.list_unspent_resources(state.channel, state.node_id)

    {:reply, unspent_resources, state}
  end

  def handle_call({:get_blocks, direction, offset}, _from, state) do
    blocks =
      RPC.get_blocks(state.channel, state.node_id, direction, offset)

    {:reply, blocks, state}
  end

  def handle_call(:get_latest_block, _from, state) do
    block = RPC.latest_block(state.channel, state.node_id)

    {:reply, block, state}
  end

  def handle_call(:get_root, _from, state) do
    block = RPC.root_block(state.channel, state.node_id)

    {:reply, block, state}
  end

  def handle_call({:filter, filters}, _from, state) do
    block = RPC.filter(state.channel, state.node_id, filters)

    {:reply, block, state}
  end

  @impl true
  # the connection to the remote node was closed.
  def handle_info({:gun_down, _pid, :http2, :closed, []}, state) do
    {:stop, :disonnect, state}
  end
end
