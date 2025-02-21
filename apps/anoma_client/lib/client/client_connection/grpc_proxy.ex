defmodule Anoma.Client.Connection.GRPCProxy do
  use GenServer
  use TypedStruct

  alias Anoma.Proto.IntentpoolService
  alias Anoma.Proto.Intentpool
  alias Anoma.Proto.Intentpool.Intent
  alias Anoma.Proto.Mempool
  alias Anoma.Proto.Node
  alias Anoma.Proto.MempoolService
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

  @spec list_intents() :: {:ok, Intentpool.List.Response.t()}
  def list_intents() do
    GenServer.call(__MODULE__, {:list_intents})
  end

  @spec add_intent(Intent.t()) :: {:ok, Intentpool.Add.Response.t()}
  def add_intent(intent) do
    GenServer.call(__MODULE__, {:add_intent, intent})
  end

  def add_transaction(jammed_nock) do
    GenServer.call(__MODULE__, {:add_transaction, jammed_nock})
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

  def handle_call({:add_transaction, jammed_nock}, _from, state) do
    node_info = %Node{id: state.node_id}

    request = %Mempool.Add.Request{
      transaction: jammed_nock,
      node: node_info
    }

    MempoolService.Stub.add(state.channel, request)
    {:reply, :ok, state}
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
