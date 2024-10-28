defmodule Anoma.Client do
  @moduledoc """
  Documentation for `Client`.
  """
  use TypedStruct

  alias Anoma.Client
  alias Anoma.Client.Connection
  alias Anoma.Client.ConnectionSupervisor

  typedstruct do
    field(:type, :grpc | :tcp)
    field(:supervisor, pid())
    field(:grpc_port, integer())
  end

  # some change
  # some change
  # some change
  @doc """
  I connect to a remote node over GRPC.
  """
  @spec connect(String.t(), integer(), integer()) ::
          {:ok, Client.t()} | {:error, term()} | {:error, term(), term()}
  def connect(host, port, listen_port) do
    spec =
      {Connection.Supervisor,
       [host: host, port: port, type: :grpc, listen_port: listen_port]}

    case DynamicSupervisor.start_child(ConnectionSupervisor, spec) do
      {:ok, pid} ->
        client_grpc_port = :ranch.get_port(<<"Anoma.Client.Api.Endpoint">>)

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

  @doc """
  Given a Client, I disconnect it and cleanup.
  """
  @spec disconnect(Client.t()) :: :ok
  def disconnect(client) do
    Supervisor.stop(client.supervisor)
  end
end
