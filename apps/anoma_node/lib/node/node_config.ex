defmodule Anoma.Node.Config do
  @moduledoc """
  I represent the configuration parameters to start a node.

  I contain information about its id, seed nodes, etc.
  """

  alias Anoma.Node.Config
  alias Anoma.Node.Config.Instance
  alias Anoma.Node.Transport.NetworkRegister.Advert

  use TypedStruct

  typedstruct enforce: true do
    field(:node_id, String.t(), required: true)
    field(:instance, Instance.t(), required: true)
    field(:seed_nodes, %{String.t() => Advert.t()}, default: %{})
  end

  typedstruct module: Instance, enforce: true do
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
  @spec instance :: Instance.t()
  def instance() do
    %Instance{
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
  @spec node(String.t()) :: Config.t()
  def node(node_id \\ random_id()) do
    %Config{
      instance: instance(),
      node_id: node_id
    }
  end

  # @doc """
  # I generate a random node id.
  # """
  @spec random_id :: String.t()
  defp random_id do
    "node_#{:erlang.phash2(make_ref())}"
  end
end
