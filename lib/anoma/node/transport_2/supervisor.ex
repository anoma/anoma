defmodule Anoma.Node.Transport2.Supervisor do
  @moduledoc """
  I am the transport supervisor.

  My main functionality is to supervise the physical transport connections
  in this node (e.g., TCP connections).

  ## Registry

  The registry maps process ids to names. For each external node connection
  a value is put into the register where the key is the remote key and the
  type of connection. For example, a TCP connection would be stored as
  `%{remote_id: remote_id, type: :tcp}` and a remote engine proxy is
  stored as `%{remote_id: remote_id, type: :router}`.

  ## TCP Supervisor

  The TCP supervisor is a dynamic supervisor that supervises TCP connections.

  ## Proxy Supervisor

  The proxy supervisor is a dynamic supervisor that supervises proxy engine
  processes. A proxy engine is a process that acts as a message relay for a
  remote engine.

  The proxy engine can receive messages and knows which physical connection
  should be used to relay the message to the remote engine.
  """
  use Supervisor

  alias Anoma.Node.Transport2.TCPSupervisor

  def start_link(init_arg) do
    Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  @impl true
  def init(_init_arg) do
    children = [
      {Registry, keys: :unique, name: ProxyRegister},
      {DynamicSupervisor,
       name: TCPSupervisor,
       strategy: :one_for_one,
       max_restarts: 1_000_000,
       max_seconds: 1},
      {DynamicSupervisor,
       name: ProxySupervisor,
       strategy: :one_for_one,
       max_restarts: 1_000_000,
       max_seconds: 1}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
