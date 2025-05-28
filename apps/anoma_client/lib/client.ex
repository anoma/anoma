defmodule Anoma.Client do
  @moduledoc """
  Documentation for `Client`.
  """

  alias Anoma.Client
  alias Anoma.Client.ConnectionSupervisor
  alias Anoma.Client.Node.Connection
  alias Anoma.Client.Node.GRPCProxy
  alias Anoma.Client.Runner
  alias Anoma.Client.Transactions
  alias Phoenix.PubSub

  use TypedStruct

  typedstruct do
    field(:pid, pid())
  end

  @doc """
  I connect to a remote node over GRPC.
  """
  @spec connect(String.t(), integer(), String.t()) ::
          {:ok, Client.t()}
          | {:error, :node_unreachable}
          | {:error, :unknown_error, any()}
  def connect(host, port, node_id) do
    # generate a unique client id
    client_id = "client_#{:rand.uniform(1000)}"

    # local port of the grpc endpoint of the client
    grpc_port = Application.get_env(:anoma_client, :grpc_port)

    conn_args = [
      host: host,
      port: port,
      node_id: node_id,
      grpc_port: grpc_port,
      client_id: client_id
    ]

    spec = {Connection.Supervisor, conn_args}

    case DynamicSupervisor.start_child(ConnectionSupervisor, spec) do
      {:ok, pid} ->
        {:ok, %Client{pid: pid}}

      {:error, {_, {:failed_to_start_child, _, {:already_started, pid}}}} ->
        {:ok, %Client{pid: pid}}

      {:error, {_, {_, _, :node_unreachable}}} ->
        {:error, :node_unreachable}

      err ->
        {:error, :unknown_error, err}
    end
  end

  @doc """
  Given a Client, I disconnect it and cleanup.
  """
  @spec disconnect(Client.t()) :: :ok
  def disconnect(client) do
    Supervisor.stop(client.pid)
  end

  @doc """
  I return the list of intents in the node I'm connected to.
  """
  @spec list_intents ::
          {:ok, [binary()]}
          | {:error, :failed_to_fetch_intents}
  def list_intents do
    GRPCProxy.list_intents()
  end

  @doc """
  I return the list of intents in the node I'm connected to.
  """
  @spec add_intent(Noun.t()) :: {:ok, :added} | {:error, String.t()}
  def add_intent(intent) do
    intent_jammed =
      intent
      |> Noun.Nounable.to_noun()
      |> Noun.Jam.jam()

    GRPCProxy.add_intent(intent_jammed)
  end

  @doc """
  I add a transaction to the mempool of the node I'm connected to.
  """
  @spec add_transaction(Noun.t(), atom()) ::
          {:ok, :added} | {:error, String.t()}
  def add_transaction(transaction, transaction_type) do
    transaction =
      transaction
      |> Noun.Nounable.to_noun()
      |> Noun.Jam.jam()

    GRPCProxy.add_transaction(transaction, transaction_type)
  end

  @doc """
  I run a Nock program with its inputs, and return the result.
  """
  @spec run(Noun.t(), [Noun.t()]) ::
          {:ok, Noun.t(), [Noun.t()]} | {:error, any(), [Noun.t()]}
  def run(program, inputs) do
    Runner.prove(program, inputs)
  end

  @doc """
  I prove a Nock program with its public and private inputs, and return the result.
  """
  @spec prove(Noun.t(), [Noun.t()], [Noun.t()]) ::
          {:ok, Noun.t(), [Noun.t()]} | {:error, any(), [Noun.t()]}
  def prove(program, public_inputs, private_inputs) do
    Runner.prove(program, public_inputs ++ private_inputs)
  end

  @doc """
  I prove a Nock program with its public and private inputs, and return the result.
  """
  @spec subscribe(String.t()) ::
          {:ok, :subscribed} | {:error, :subscribe_failed, any()}
  def subscribe(topic) do
    # subscribe to the phoenix pubsub to receive events from the remote node
    PubSub.subscribe(:client_pubsub, "node_events")
    # subscribe on the remote node itself
    GRPCProxy.subscribe(topic)
  end

  @doc """
  I compose a list of transactions.
  """
  def compose(transactions) do
    Transactions.compose(transactions)
  end
end
