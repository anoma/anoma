defmodule Anoma.Node.Transport.Discovery do
  @moduledoc """
  I contain logic to handle the discovery of new nodes on the network.

  When a new node connects via TCP or other network connections, I ensure that the proper messages
  are sent to that node.

  When a new node is discovered, I also create an engine proxy for that node.

  ### Public API

  I provide the following public functionality:

  - `announce_self/2`
  - `create_node_proxy/2`
  """
  alias Anoma.Crypto.Id
  alias Anoma.Node.Transport.Router

  require Logger

  @doc """
  I send a remote node a message with our node meta data over the connection process given to me.
  """
  @spec announce_self(GenServer.server(), Id.t()) :: :ok
  def announce_self(connection_pid, local_node_id) do
    discovery_message = %{
      type: :inform,
      node_id: local_node_id
    }

    # todo: this should maybe go via the router, rather than directly to the tcp connection?
    GenServer.cast(connection_pid, {:send, discovery_message})

    :ok
  end

  @doc """
  Given a remote router id, I create a proxy engine for that remote router.
  This proxy can then be used to send messages to the remote router.

  Note: this proxy does not have to know the tcp connection to the remote router.
        the proxy will lookup the connection in the registry to see if its available.
        This allows the registry to be the single source of truth for all connections.
        If the connection is down, the registry will not have this process anymore, and the proxy
        will not be able to send messages to the remote router.
  """
  @spec create_node_proxy(Id.t(), Id.t()) :: :ok
  def create_node_proxy(remote_node_id, local_node_id) do
    Logger.debug("creating proxy for remote node #{inspect(remote_node_id)}")

    Router.start_proxy_engine(remote_node_id,
      node_id: local_node_id,
      type: Router
    )
  end
end
