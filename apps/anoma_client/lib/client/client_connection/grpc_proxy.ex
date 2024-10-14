defmodule Anoma.Client.Connection.GRPCProxy do
  use GenServer
  use TypedStruct

  alias Anoma.Protobuf.Indexer.Nullifiers
  alias Anoma.Protobuf.Indexer.UnrevealedCommits
  alias Anoma.Protobuf.Indexer.UnspentResources
  alias Anoma.Protobuf.IntentPool.AddIntent
  alias Anoma.Protobuf.IntentPool.ListIntents
  alias Anoma.Protobuf.Intents.Stub
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
    """
    field(:port, integer())
    field(:host, String.t())
    field(:channel, any())
  end

  ############################################################
  #                    Genserver Helpers                     #
  ############################################################

  def start_link(args) do
    args = Keyword.validate!(args, [:port, :host])
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

  @spec list_intents() :: {:ok, Anoma.Protobuf.IntentPool.ListIntents.Response.t()}
  def list_intents() do
    GenServer.call(__MODULE__, {:list_intents})
  end

  @spec add_intent(Anoma.Protobuf.Intent.t()) ::
          {:ok, Anoma.Protobuf.IntentPool.AddIntent.Response.t()}
  def add_intent(intent) do
    GenServer.call(__MODULE__, {:add_intent, intent})
  end

  @spec list_nullifiers() :: {:ok, Anoma.Protobuf.Indexer.Nullifiers.Response.t()}
  def list_nullifiers() do
    GenServer.call(__MODULE__, {:list_nullifiers})
  end

  @spec list_unrevealed_commits() ::
          {:ok, Anoma.Protobuf.Indexer.UnrevealedCommits.Response.t()}
  def list_unrevealed_commits() do
    GenServer.call(__MODULE__, {:list_unrevealed_commits})
  end

  @spec list_unspent_resources() ::
          {:ok, Anoma.Protobuf.Indexer.UnspentResources.Response.t()}
  def list_unspent_resources() do
    GenServer.call(__MODULE__, {:list_unspent_resources})
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  @impl true
  def handle_call({:list_intents}, _from, state) do
    request = %ListIntents.Request{}
    intents = Stub.list_intents(state.channel, request)
    {:reply, intents, state}
  end

  def handle_call({:add_intent, intent}, _from, state) do
    request = %AddIntent.Request{intent: intent}
    result = Stub.add_intent(state.channel, request)
    {:reply, result, state}
  end

  def handle_call({:list_nullifiers}, _from, state) do
    request = %Nullifiers.Request{}
    nullifiers = Stub.list_nullifiers(state.channel, request)
    {:reply, nullifiers, state}
  end

  def handle_call({:list_unrevealed_commits}, _from, state) do
    request = %UnrevealedCommits.Request{}
    commits = Stub.list_unrevealed_commits(state.channel, request)
    {:reply, commits, state}
  end

  def handle_call({:list_unspent_resources}, _from, state) do
    request = %UnspentResources.Request{}
    resources = Stub.list_unspent_resources(state.channel, request)
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
end
