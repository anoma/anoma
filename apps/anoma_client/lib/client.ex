defmodule Anoma.Client do
  @moduledoc """
  Documentation for `Client`.
  """
  use TypedStruct

  alias Anoma.Client
  alias Anoma.Client.Node.Connection
  alias Anoma.Client.ConnectionSupervisor
  alias Anoma.Client.Node.GRPCProxy
  alias Anoma.Client.Nock.Runner

  typedstruct do
    field(:pid, pid())
  end

  @doc """
  I connect to a remote node over GRPC.
  """
  @spec connect(String.t(), integer(), String.t()) ::
          {:ok, Client.t()}
          | {:error, :node_unreacable}
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
  @spec add_transaction(Noun.t()) :: {:ok, :added} | {:error, String.t()}
  def add_transaction(transaction) do
    transaction =
      transaction
      |> Noun.Nounable.to_noun()
      |> Noun.Jam.jam()

    GRPCProxy.add_transaction(transaction)
  end

  @doc """
  I run a Nock program with its inputs, and return the result.
  """
  @spec run(Noun.t(), [Noun.t()]) :: {:ok, Noun.t()} | {:error, any()}
  def run(program, inputs) do
    Runner.prove(program, inputs)
  end

  @doc """
  I prove a Nock program with its public and private inputs, and return the result.
  """
  @spec prove(Noun.t(), [Noun.t()], [Noun.t()]) ::
          {:ok, Noun.t()} | {:error, any()}
  def prove(program, public_inputs, private_inputs) do
    Runner.prove(program, public_inputs ++ private_inputs)
  end

  @doc """
  I prove a Nock program with its public and private inputs, and return the result.
  """
  @spec subscribe(String.t()) ::
          {:ok, :subscribed} | {:error, :subscribe_failed, any()}
  def subscribe(topic) do
    GRPCProxy.subscribe(topic)
  end

  # @doc """
  # I return the list of intents in the node I'm connected to.
  # """
  # @spec add_intent(Transaction.t()) :: any()
  # def add_intent(intent) do
  #   intent_jammed =
  #     intent
  #     |> Noun.Nounable.to_noun()
  #     |> Noun.Jam.jam()

  #   intent = %Intent{intent: intent_jammed}
  #   {:ok, result} = GRPCProxy.add_intent(intent)
  #   result.result
  # end

  # @doc """
  # I return the list of intents in the node I'm connected to.
  # """
  # @spec list_nullifiers :: any()
  # def list_nullifiers do
  #   {:ok, result} = GRPCProxy.list_nullifiers()
  #   result.nullifiers
  # end

  # @doc """
  # I return the list of intents in the node I'm connected to.
  # """
  # @spec list_unrevealed_commits :: any()
  # def list_unrevealed_commits do
  #   {:ok, result} = GRPCProxy.list_unrevealed_commits()
  #   result.commits
  # end

  # @doc """
  # I return the list of intents in the node I'm connected to.
  # """
  # @spec list_unspent_resources :: any()
  # def list_unspent_resources do
  #   {:ok, result} = GRPCProxy.list_unspent_resources()
  #   result.unspent_resources
  # end
end
