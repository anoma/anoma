defmodule Anoma.Client.Connection.GRPCProxy do
  use GenServer
  use TypedStruct

  alias Anoma.Protobuf.Indexer.Nullifiers
  alias Anoma.Protobuf.Indexer.UnrevealedCommits
  alias Anoma.Protobuf.Indexer.UnspentResources
  alias Anoma.Protobuf.IndexerService
  alias Anoma.Protobuf.Intents.Add
  alias Anoma.Protobuf.Intents.Intent
  alias Anoma.Protobuf.Intents.List
  alias Anoma.Protobuf.IntentsService
  alias Anoma.Protobuf.NodeInfo
  alias Anoma.Protobuf.Mempool.AddTransaction
  alias Anoma.Protobuf.MempoolService
  alias Anoma.Protobuf.Executor.AddROTransaction
  alias Anoma.Protobuf.ExecutorService
  require Logger

  ############################################################
  #                    State                                 #
  ############################################################

  typedstruct do
    @typedoc """
    I am the state of a TCP listener.

    My fields contain information to listen for TCP connection with a remote node.

    ### Fields
    - `:port`    - The port on which the remote node is listening to GRPC.
    - `:host`    - The host on which the remote node is listening to GRPC.
    - `:channel` - The channel to the remote grpc.
    - `:node_id` - The id of the remote node.
    """
    field(:port, integer())
    field(:host, String.t())
    field(:channel, any())
    field(:node_id, String.t())
  end

  ############################################################
  #                    Genserver Helpers                     #
  ############################################################

  @spec start_link(Keyword.t()) :: GenServer.on_start()
  def start_link(args) do
    args = Keyword.validate!(args, [:port, :host, :node_id])
    GenServer.start_link(__MODULE__, args, name: __MODULE__)
  end

  @impl true
  def init(args) do
    state = struct(__MODULE__, Enum.into(args, %{}))

    case GRPC.Stub.connect("#{state.host}:#{state.port}") do
      {:ok, channel} ->
        {:ok, %{state | channel: channel}}

      _err ->
        {:stop, :node_unreachable}
    end
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @spec list_intents() :: {:ok, List.Response.t()}
  def list_intents() do
    GenServer.call(__MODULE__, {:list_intents})
  end

  @spec add_intent(Intent.t()) :: {:ok, Add.Response.t()}
  def add_intent(intent) do
    GenServer.call(__MODULE__, {:add_intent, intent})
  end

  @spec list_nullifiers() :: {:ok, Nullifiers.Response.t()}
  def list_nullifiers() do
    GenServer.call(__MODULE__, {:list_nullifiers})
  end

  @spec list_unrevealed_commits() :: {:ok, UnrevealedCommits.Response.t()}
  def list_unrevealed_commits() do
    GenServer.call(__MODULE__, {:list_unrevealed_commits})
  end

  @spec list_unspent_resources() :: {:ok, UnspentResources.Response.t()}
  def list_unspent_resources() do
    GenServer.call(__MODULE__, {:list_unspent_resources})
  end

  def add_transaction(jammed_nock) do
    GenServer.call(__MODULE__, {:add_transaction, jammed_nock})
  end

  @spec add_read_only_transaction(binary()) ::
          {:ok, AddROTransaction.Response.t()}
  def add_read_only_transaction(jammed_nock) do
    GenServer.call(__MODULE__, {:add_ro_transaction, jammed_nock})
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  @impl true
  def handle_call({:list_intents}, _from, state) do
    node_info = %NodeInfo{node_id: state.node_id}
    request = %List.Request{node_info: node_info}
    intents = IntentsService.Stub.list_intents(state.channel, request)
    {:reply, intents, state}
  end

  def handle_call({:add_intent, intent}, _from, state) do
    node_info = %NodeInfo{node_id: state.node_id}
    request = %Add.Request{node_info: node_info, intent: intent}
    result = IntentsService.Stub.add_intent(state.channel, request)
    {:reply, result, state}
  end

  def handle_call({:list_nullifiers}, _from, state) do
    node_info = %NodeInfo{node_id: state.node_id}

    request = %Nullifiers.Request{node_info: node_info}
    nullifiers = IndexerService.Stub.list_nullifiers(state.channel, request)
    {:reply, nullifiers, state}
  end

  def handle_call({:list_unrevealed_commits}, _from, state) do
    node_info = %NodeInfo{node_id: state.node_id}
    request = %UnrevealedCommits.Request{node_info: node_info}

    commits =
      IndexerService.Stub.list_unrevealed_commits(state.channel, request)

    {:reply, commits, state}
  end

  def handle_call({:list_unspent_resources}, _from, state) do
    node_info = %NodeInfo{node_id: state.node_id}
    request = %UnspentResources.Request{node_info: node_info}

    resources =
      IndexerService.Stub.list_unspent_resources(state.channel, request)

    {:reply, resources, state}
  end

  def handle_call({:add_transaction, jammed_nock}, _from, state) do
    node_info = %NodeInfo{node_id: state.node_id}

    request = %AddTransaction.Request{
      transaction: jammed_nock,
      node_info: node_info
    }

    MempoolService.Stub.add(state.channel, request)
    {:reply, :ok, state}
  end

  def handle_call({:add_ro_transaction, jammed_nock}, _from, state) do
    node_info = %NodeInfo{node_id: state.node_id}

    request = %AddROTransaction.Request{
      transaction: jammed_nock,
      node_info: node_info
    }

    response = ExecutorService.Stub.add(state.channel, request)
    {:reply, response, state}
  end

  @impl true
  def handle_cast(_message, state) do
    {:noreply, state}
  end

  @impl true
  def handle_info(_message, state) do
    {:noreply, state}
  end
end
