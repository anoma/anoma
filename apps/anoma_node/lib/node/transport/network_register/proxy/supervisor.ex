defmodule Anoma.Node.Transport.Proxy.Supervisor do
  @moduledoc """
  I am the supervisor for a proxy to a specific node.

  I supervise the processes that communicate with that node, and listen for
  messages that have to be sent to that node.

  ## Node Proxy

  The node proxy is the main process that acts as a proxy for a remote node.

  ## Engine Proxy

  For each engine at the remote node, an engine proxy is created locally. This
  proxy will intercept messages addressed to a remote engine and send it to the
  remote node via the appropriate protocol.

  ## Transport Protocol Supervisor

  The transport protocol supervisor manages all the transport protocol engines
  for the remote node.

  If a remote node is reachable via, e.g., grpc and tcp, there will be a
  transport protocol for both of these protocols.
  """
  use Supervisor

  require Logger

  alias Anoma.Node.Registry
  alias Anoma.Node.Transport.Proxy
  alias Anoma.Node.Transport.Proxy.TransportProtocol
  alias Anoma.Node.Transaction.Mempool

  ############################################################
  #                       Types                              #
  ############################################################

  @typep startup_options() :: [
           {:node_id, String.t()},
           {:remote_node_id, String.t()}
         ]

  ############################################################
  #                      Supervisor Callbacks                #
  ############################################################

  @spec start_link(startup_options) :: Supervisor.on_start()
  def start_link(args) do
    name = Registry.via(args[:remote_node_id], __MODULE__)
    Supervisor.start_link(__MODULE__, args, name: name)
  end

  @impl true
  @spec init(startup_options) :: {:ok, any()} | :ignore
  def init(args) do
    Logger.debug("#{inspect(self())} proxy supervisor #{inspect(args)}")
    Process.set_label(__MODULE__)

    children = [
      # the proxy for the node
      {Proxy.Node, args},
      # proxy for the engines on the remote node
      {Proxy.Engine, [{:engine, Mempool} | args]},
      # supervisor for transport protocols for this node
      {
        DynamicSupervisor,
        name:
          Registry.via(args[:remote_node_id], TransportProtocol.Supervisor),
        strategy: :one_for_one
      }
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
