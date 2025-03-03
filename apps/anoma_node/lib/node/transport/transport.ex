defmodule Anoma.Node.Transport do
  alias Anoma.Node.Transport.EngineProxy
  alias Anoma.Node.Transport.TCP

  ############################################################
  #                    Types                                 #
  ############################################################

  @typedoc """
  Shorthand type for socket.
  """
  @type hostname :: :inet.socket_address() | :inet.hostname()

  @typedoc """
  Shorthand type for port number.
  """
  @type port_number :: :inet.port_number()

  ############################################################
  #                      Public API                          #
  ############################################################

  @doc """
  I start a new TCP server for the given node.
  """
  @spec start_tcp_server(String.t(), {hostname, port_number}) ::
          {:ok, pid, port_number} | {:error, term}
  def start_tcp_server(node_id, {host, port} \\ {{0, 0, 0, 0}, 0}) do
    supervisor = Anoma.Node.Registry.whereis(node_id, :tcp_supervisor)
    args = [node_id: node_id, host: host, port: port]

    case DynamicSupervisor.start_child(supervisor, {TCP.Listener, args}) do
      {:ok, pid} ->
        port = TCP.Listener.port(pid)
        {:ok, pid, port}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  I start a new TCP client connection to a given host and port.
  """
  @spec start_tcp_client(String.t(), {hostname, port_number}) ::
          {:ok, pid, port_number} | {:error, term}
  def start_tcp_client(node_id, {host, port}) do
    # attempt to conenct to the remote node
    socket_opts = [
      :binary,
      active: false,
      exit_on_close: true,
      reuseaddr: true,
      ifaddr: host
    ]

    case :gen_tcp.connect(host, port, socket_opts) do
      {:ok, socket} ->
        supervisor = Anoma.Node.Registry.whereis(node_id, :tcp_supervisor)
        args = [node_id: node_id, socket: socket]

        case DynamicSupervisor.start_child(supervisor, {TCP.Connection, args}) do
          {:ok, pid} ->
            :gen_tcp.controlling_process(socket, pid)
            :inet.setopts(socket, [{:active, true}])
            {:ok, pid, port}

          {:error, reason} ->
            {:error, reason}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Given a remote node id, I start a proxy engine that represents that remote node.
  """
  @spec start_engine_proxy(String.t(), String.t(), atom()) ::
          {:ok, pid} | {:error, term}
  def start_engine_proxy(node_id, remote_node_id, type) do
    supervisor = Anoma.Node.Registry.whereis(node_id, :proxy_supervisor)
    args = [node_id: node_id, remote_node_id: remote_node_id, type: type]

    DynamicSupervisor.start_child(supervisor, {EngineProxy, args})
  end
end
