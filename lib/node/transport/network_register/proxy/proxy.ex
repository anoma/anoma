defmodule Anoma.Node.Transport.Proxy do
  alias Anoma.Node.Registry
  alias Anoma.Node.Transport
  alias Anoma.Node.Transport.Proxy
  alias Anoma.Node.Transport.Proxy.TransportProtocol
  alias Anoma.Node.Transport.NetworkRegister.Advert

  require Logger

  @doc """
  I create a node proxy for the given remote node id.
  """
  @spec create(String.t(), String.t(), Advert.t()) ::
          {:ok, :created} | {:error, any()}
  def create(local_node_id, remote_node_id, advert) do
    Logger.debug("create proxy node for #{inspect(binding())}")

    # lookup the supervisor for this node's proxies.
    supervisor = Registry.whereis(local_node_id, Transport.ProxySupervisor)

    # arguments for the node proxy
    proxy_args = [node_id: local_node_id, remote_node_id: remote_node_id]

    # create the node proxy. It might already exists, and in that case I return
    # :ok as well
    spec = {Proxy.Supervisor, proxy_args}

    case DynamicSupervisor.start_child(supervisor, spec) do
      {:ok, _pid} ->
        {:ok, :created}

      {:error, {:already_started, _pid}} ->
        Logger.debug("node proxy already started")
        {:ok, :created}

      {:error, e} ->
        {:error, e}
    end
  end

  @doc """
  I create a transport protocol for the given remote node and its parameters.
  """
  @spec create_transport_protocol(
          String.t(),
          String.t(),
          Advert.t()
        ) :: :ok | {:error, any()}
  def create_transport_protocol(node_id, remote_node_id, advert) do
    Logger.debug("create transport protocol for #{inspect(binding())}")

    {:ok, _} = create_proxy_for(node_id, remote_node_id, advert.grpc_address)
    {:ok, _} = create_proxy_for(node_id, remote_node_id, advert.tcp_address)
    :ok
  end

  # create a proxy for an address
  # does nothing if the address is nil
  defp create_proxy_for(_, _, nil), do: {:ok, :not_created}

  defp create_proxy_for(node_id, remote_node_id, address) do
    Logger.debug("create transport protocol for #{inspect(binding())}")

    # lookup the supervisor for this node's proxies.
    supervisor =
      Registry.whereis(remote_node_id, TransportProtocol.Supervisor)

    # arguments for the transport protocol
    transport_protocol_args = [
      node_id: node_id,
      remote_node_id: remote_node_id,
      address: address
    ]

    # create the node proxy. It might already exists, and in that case I return
    # :ok as well
    spec = {TransportProtocol, transport_protocol_args}

    case DynamicSupervisor.start_child(supervisor, spec) do
      {:ok, _pid} ->
        {:ok, :created}

      {:error, {:already_started, _pid}} ->
        {:ok, :created}

      {:error, e} ->
        {:error, e}
    end
  end
end
