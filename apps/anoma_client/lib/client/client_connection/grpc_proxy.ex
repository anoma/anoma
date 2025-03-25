defmodule Anoma.Client.Connection.GRPCProxy do
  alias Anoma.Client.Examples.EClient.Mempool
  alias Anoma.Proto.Executor.AddROTransaction
  alias Anoma.Proto.ExecutorService
  alias Anoma.Proto.Intentpool
  alias Anoma.Proto.Intentpool.Intent
  alias Anoma.Proto.IntentpoolService
  alias Anoma.Proto.Mempool
  alias Anoma.Proto.MempoolService
  alias Anoma.Proto.IndexerService
  alias Anoma.Proto.Node
  alias Anoma.Proto.Indexer.Commits
  alias Anoma.Proto.Indexer.FilterResource
  alias Anoma.Proto.Indexer.GetBlock
  alias Anoma.Proto.Indexer.LatestBlock
  alias Anoma.Proto.Indexer.Nullifiers
  alias Anoma.Proto.Indexer.RootBlock
  alias Anoma.Proto.Indexer.UnrevealedCommits
  alias Anoma.Proto.Indexer.UnspentResources

  require Logger

  use GenServer
  use TypedStruct

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

    case connect(state.host, state.port) do
      {:ok, channel} ->
        {:ok, %{state | channel: channel}}

      {:error, :failed_to_connect} ->
        {:stop, :node_unreachable}
    end
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @spec list_intents() :: {:ok, Intentpool.List.Response.t()}
  def list_intents() do
    GenServer.call(__MODULE__, {:list_intents})
  end

  @spec add_intent(Intent.t()) :: {:ok, Intentpool.Add.Response.t()}
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

  @spec list_commits() :: {:ok, Commits.Response.t()}
  def list_commits() do
    GenServer.call(__MODULE__, {:list_commits})
  end

  @spec list_unspent_resources() :: {:ok, UnspentResources.Response.t()}
  def list_unspent_resources() do
    GenServer.call(__MODULE__, {:list_unspent_resources})
  end

  @spec add_transaction(binary(), atom()) :: {:ok, Mempool.Add.Response.t()}
  def add_transaction(jammed_nock, transaction_type) do
    GenServer.call(
      __MODULE__,
      {:add_transaction, jammed_nock, transaction_type}
    )
  end

  @spec add_read_only_transaction(binary()) ::
          {:ok, AddROTransaction.Response.t()}
  def add_read_only_transaction(jammed_nock) do
    GenServer.call(__MODULE__, {:add_ro_transaction, jammed_nock})
  end

  @spec get_blocks({:before | :after, non_neg_integer()}) ::
          {:ok, GetBlock.Response.t()}
  def get_blocks({direction, offset}) do
    GenServer.call(__MODULE__, {:get_blocks, direction, offset})
  end

  @spec get_latest_block() :: {:ok, LatestBlock.Response.t()}
  def get_latest_block() do
    GenServer.call(__MODULE__, :get_latest_block)
  end

  @spec root() :: {:ok, RootBlock.Response.t()}
  def root() do
    GenServer.call(__MODULE__, :get_root)
  end

  @spec filter([{atom, any()}]) :: {:ok, FilterResource.Response.t()}
  def filter(filters) do
    GenServer.call(__MODULE__, {:filter, filters})
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  @impl true
  def handle_call({:list_intents}, _from, state) do
    node_info = %Node{id: state.node_id}
    request = %Intentpool.List.Request{node: node_info}
    intents = IntentpoolService.Stub.list(state.channel, request)
    {:reply, intents, state}
  end

  def handle_call({:add_intent, intent}, _from, state) do
    node_info = %Node{id: state.node_id}
    request = %Intentpool.Add.Request{node: node_info, intent: intent}

    result = IntentpoolService.Stub.add(state.channel, request)
    {:reply, result, state}
  end

  def handle_call({:list_nullifiers}, _from, state) do
    node_info = %Node{id: state.node_id}

    request = %Nullifiers.Request{node: node_info}
    nullifiers = IndexerService.Stub.list_nullifiers(state.channel, request)
    {:reply, nullifiers, state}
  end

  def handle_call({:list_unrevealed_commits}, _from, state) do
    node_info = %Node{id: state.node_id}
    request = %UnrevealedCommits.Request{node: node_info}

    commits =
      IndexerService.Stub.list_unrevealed_commits(state.channel, request)

    {:reply, commits, state}
  end

  def handle_call({:list_commits}, _from, state) do
    node_info = %Node{id: state.node_id}
    request = %Commits.Request{node: node_info}

    commits = IndexerService.Stub.list_commits(state.channel, request)

    {:reply, commits, state}
  end

  def handle_call({:list_unspent_resources}, _from, state) do
    node_info = %Node{id: state.node_id}
    request = %UnspentResources.Request{node: node_info}

    resources =
      IndexerService.Stub.list_unspent_resources(state.channel, request)

    {:reply, resources, state}
  end

  def handle_call({:add_transaction, jammed_nock, type}, _from, state) do
    node_info = %Node{id: state.node_id}

    request = %Mempool.Add.Request{
      transaction: %Mempool.Transaction{transaction: jammed_nock},
      node: node_info,
      transaction_type: type
    }

    result = MempoolService.Stub.add(state.channel, request)
    {:reply, result, state}
  end

  def handle_call({:add_ro_transaction, jammed_nock}, _from, state) do
    node_info = %Node{id: state.node_id}

    request = %AddROTransaction.Request{
      transaction: %Mempool.Transaction{transaction: jammed_nock},
      node: node_info
    }

    response = ExecutorService.Stub.add(state.channel, request)
    {:reply, response, state}
  end

  def handle_call({:get_blocks, direction, offset}, _from, state) do
    node_info = %Node{id: state.node_id}
    request = %GetBlock.Request{node: node_info, index: {direction, offset}}
    blocks = IndexerService.Stub.get_block(state.channel, request)
    {:reply, blocks, state}
  end

  def handle_call(:get_latest_block, _from, state) do
    node_info = %Node{id: state.node_id}

    request = %LatestBlock.Request{
      node: node_info
    }

    latest_block = IndexerService.Stub.latest_block(state.channel, request)
    {:reply, latest_block, state}
  end

  def handle_call(:get_root, _from, state) do
    node_info = %Node{id: state.node_id}

    request = %RootBlock.Request{node: node_info}

    root = IndexerService.Stub.root_block(state.channel, request)
    {:reply, root, state}
  end

  def handle_call({:filter, filters}, _from, state) do
    node_info = %Node{id: state.node_id}

    request = %FilterResource.Request{node: node_info, filters: filters}

    resources = IndexerService.Stub.filter_resource(state.channel, request)
    {:reply, resources, state}
  end

  @impl true
  def handle_cast(_message, state) do
    {:noreply, state}
  end

  @impl true
  def handle_info(_message, state) do
    {:noreply, state}
  end

  ############################################################
  #                       Helpers                            #
  ############################################################

  # @doc """
  # I try to establish a connection to the remote node.
  # If I dont succeed after 10 attempts, I stop trying.
  # """
  @spec connect(String.t(), String.t(), non_neg_integer()) ::
          {:ok, any()} | {:error, :failed_to_connect}
  defp connect(host, port, attempts \\ 5)

  defp connect(host, port, 0) do
    Logger.error("failed to connect to node at #{host}, port #{port}")
    {:error, :failed_to_connect}
  end

  defp connect(host, port, attempts) do
    Logger.debug("connecting to node at #{host}, port #{port}")

    case GRPC.Stub.connect("#{host}:#{port}") do
      {:ok, channel} ->
        {:ok, channel}

      _err ->
        connect(host, port, attempts - 1)
    end
  end
end
