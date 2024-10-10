defmodule Anoma.Client do
  @moduledoc """
  Documentation for `Client`.
  """
  use TypedStruct

  typedstruct do
    field(:type, :grpc | :tcp)
    field(:supervisor, pid())
  end

  @doc """
  I connect to a remote node over GRPC.
  """
  @spec connect(String.t(), integer()) :: {:ok, Anoma.Client.t()} | {:error, term()}
  def connect(host, port) do
    spec =
      {Anoma.Client.Connection.Supervisor, [host: host, port: port, type: :grpc]}

    case DynamicSupervisor.start_child(Anoma.Client.ConnectionSupervisor, spec) do
      {:ok, pid} ->
        {:ok, %__MODULE__{type: :grpc, supervisor: pid}}

      {:error, {_, {_, _, :node_unreachable}}} ->
        {:error, :node_unreachable}

      err ->
        IO.inspect(err)
        {:error, :unknown}
    end
  end

  @doc """
  Given a Client, I disconnect it and cleanup.
  """
  @spec disconnect(Anoma.Client.t()) :: :ok
  def disconnect(client) do
    Supervisor.stop(client.supervisor)
  end
end
