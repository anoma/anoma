defmodule Anoma.Client do
  @moduledoc """
  Documentation for `Client`.
  """
  use TypedStruct

  alias Anoma.Client
  alias Anoma.Client.Connection
  alias Anoma.Client.Connection.GRPCProxy
  alias Anoma.Client.ConnectionSupervisor
  alias Anoma.Client.Runner
  alias Anoma.Protobuf.Intents.Intent

  @client_config_dir "./tmp/clients"

  typedstruct do
    field(:type, :grpc | :tcp)
    field(:supervisor, pid())
    field(:grpc_port, integer())
  end

  @doc """
  I connect to a remote node over GRPC.
  """
  @spec connect(String.t(), integer(), integer(), String.t()) ::
          {:ok, Client.t()} | {:error, term()} | {:error, term(), term()}
  def connect(host, port, listen_port, node_id) do
    spec =
      {Connection.Supervisor,
       [
         host: host,
         port: port,
         type: :grpc,
         listen_port: listen_port,
         node_id: node_id
       ]}

    case DynamicSupervisor.start_child(ConnectionSupervisor, spec) do
      {:ok, pid} ->
        client_grpc_port = :ranch.get_port(<<"Anoma.Client.Api.Endpoint">>)

        store_client_config_file(client_grpc_port, node_id)

        {:ok,
         %__MODULE__{
           type: :grpc,
           supervisor: pid,
           grpc_port: client_grpc_port
         }}

      {:error, {_, {_, _, :node_unreachable}}} ->
        {:error, :node_unreachable}

      err ->
        {:error, :unknown_error, err}
    end
  end

  defp store_client_config_file(grpc_port, node_id) do
    config = %{grpc_port: grpc_port, node_id: node_id}
    path = Path.join(@client_config_dir, "config_#{node_id}.json")
    File.mkdir_p!(@client_config_dir)
    File.write!(path, Jason.encode!(config))
  end

  @doc """
  Given a Client, I disconnect it and cleanup.
  """
  @spec disconnect(Client.t()) :: :ok
  def disconnect(client) do
    Supervisor.stop(client.supervisor)
  end

  @doc """
  I run a Nock program with its inputs, and return the result.
  """
  @spec run_nock(Noun.t(), [Noun.t()]) ::
          {:ok, Noun.t()} | {:error, any()}
  def run_nock(program, inputs) do
    Runner.prove(program, inputs)
  end

  @doc """
  I return the list of intents in the node I'm connected to.
  """
  @spec list_intents :: [Noun.t()]
  def list_intents do
    {:ok, result} = GRPCProxy.list_intents()

    result.intents
  end

  @doc """
  I return the list of intents in the node I'm connected to.
  """
  @spec add_intent(integer()) :: any()
  def add_intent(intent) do
    intent = %Intent{value: intent}
    {:ok, result} = GRPCProxy.add_intent(intent)
    result.result
  end

  @doc """
  I return the list of intents in the node I'm connected to.
  """
  @spec list_nullifiers :: any()
  def list_nullifiers do
    {:ok, result} = GRPCProxy.list_nullifiers()
    result.nullifiers
  end

  @doc """
  I return the list of intents in the node I'm connected to.
  """
  @spec list_unrevealed_commits :: any()
  def list_unrevealed_commits do
    {:ok, result} = GRPCProxy.list_unrevealed_commits()
    result.commits
  end

  @doc """
  I return the list of intents in the node I'm connected to.
  """
  @spec list_unspent_resources :: any()
  def list_unspent_resources do
    {:ok, result} = GRPCProxy.list_unspent_resources()
    result.unspent_resources
  end

  @doc """
  I return the client config that was stored in the most recently created file.
  """
  @spec get_last_client_config :: {:ok, map()} | {:error, :no_client_config}
  def get_last_client_config do
    with {:ok, files} <- File.ls(@client_config_dir),
         false <- Enum.empty?(files),
         last_file <-
           files
           |> Enum.map(&Path.join(@client_config_dir, &1))
           |> Enum.filter(&File.exists?/1)
           |> Enum.max_by(&File.stat!(&1).mtime, fn -> nil end),
         {:ok, content} <- File.read(last_file),
         {:ok, config} <- Jason.decode(content) do
      {:ok, config}
    else
      _ -> {:error, :no_client_config}
    end
  end
end
