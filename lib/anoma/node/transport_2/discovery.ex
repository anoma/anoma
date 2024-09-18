defmodule Anoma.Node.Transport2.Discovery do
  @moduledoc """
  I contain logic to handle the discovery of new nodes on the network.

  I am used to register remote nodes locally, and to setup the necessary connections to them.
  I also register proxies locally, and create the necessary inform
  """
  alias Anoma.Crypto.Id
  alias Anoma.Node.Transport2.Router

  @doc """
  I inform a remote node about my existence.

  Given a process id of a network connection I send a message that contains
  my type, a message, and my router id.
  """
  @spec inform_node(pid(), Id.t()) :: :ok
  def inform_node(pid, router_id) do
    discovery_message = %{
      type: :inform,
      message: "HOIHOI",
      router_id: router_id
    }

    send(pid, {:send, discovery_message})
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
  def remember_node(router_id) do
    # start a proxy engine for this node
    Router.start_proxy_engine(router_id)

    # announce
  end

  @doc """
  I register the current process as the tcp connection for a given remote router id.
  If there is already a tcp registered for this remote router id I return that process.
  """
  @spec register_as_tcp_for_node(Id.t()) ::
          {:ok, pid()} | {:error, {:already_registered, pid()}}
  def register_as_tcp_for_node(remote_id) do
    # register this process as the tcp connection for the given remote_id
    key = %{remote_id: remote_id, type: :tcp}
    value = %{}
    res = Registry.register(ProxyRegister, key, value)
    IO.inspect(res)

    # announce connection to proxy engines
    GenServer.cast(
      {:via, Registry,
       {ProxyRegister, %{remote_id: remote_id, type: :router}}},
      {:new_connection, self()}
    )

    res
  end
end
