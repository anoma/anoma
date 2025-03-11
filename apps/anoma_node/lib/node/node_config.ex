defmodule Anoma.Node.Config do
  @moduledoc """
  I represent the configuration parameters to start a node.

  I contain information about its id, seed nodes, etc.
  """

  alias Anoma.Node.Config
  alias Anoma.Node.Config.RuntimeSystemConfig
  alias Anoma.Node.Replay

  use TypedStruct

  typedstruct enforce: true do
    field(:node_id, String.t(), required: true)
    field(:startup_arguments, Replay.State.startup_args(), default: [mempool: []])
    field(:runtime_system_config, RuntimeSystemConfig.t(), required: true)
  end

  typedstruct module: RuntimeSystemConfig, enforce: true do
    @typedoc """
    I contain the configuration parameters for this Anoma instance.
    These settings are shared between all nodes that run in this instance.
    """
    field(:node_grpc_port, non_neg_integer(), required: true)
    field(:node_grpc_host, String.t(), required: true)
    field(:client_grpc_port, non_neg_integer(), required: true)
    field(:client_grpc_host, String.t(), required: true)
  end

  @doc """
  I return a node config for this instance.
  These are settings that apply for all nodes that run in this instance.
  I fetch the values from the config.exs files and return a Platform struct.
  """
  @spec runtime_system_config :: RuntimeSystemConfig.t()
  def runtime_system_config() do
    %RuntimeSystemConfig{
      node_grpc_port: Application.get_env(:anoma_node, :grpc_port),
      node_grpc_host: Application.get_env(:anoma_node, :grpc_host),
      client_grpc_port: Application.get_env(:anoma_client, :grpc_port),
      client_grpc_host: Application.get_env(:anoma_client, :grpc_host)
    }
  end

  @doc """
  I return the configuration parameters for a node on this instance.
  If no node id is given I generate a random one.
  """
  @spec node(map()) :: Config.t()
  def node(params \\ %{}) do
    args =
      %{runtime_system_config: runtime_system_config(), node_id: random_id()}
      |> Map.merge(params)

    struct(Config, args)
  end

  # @doc """
  # I generate a random node id.
  # """
  @spec random_id :: String.t()
  defp random_id do
    "node_#{:erlang.phash2(make_ref())}"
  end
end
