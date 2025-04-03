defmodule Peer do
  require Logger
  @doc """
  I ensure that distribution is enabled on this node.
  """
  @spec ensure_distribution :: :ok
  def ensure_distribution do
    if node() == :nonode@nohost do
      :ok =
        case :net_kernel.start([:"benchmarker@127.0.0.1"]) do
          {:ok, _pid} ->
            :ok

          {:error, {:already_started, _}} ->
            :ok

          err ->
            Logger.error("""
            Failed to start distribution.
            This can happen if the system does not have an epmd instance running.
            Check with `ps -ax | grep epmd`, and start manually with `epmd -d &`
            """)

            err
        end
    end

    :erlang.set_cookie(:erlang.get_cookie())

    :erl_boot_server.start([to_charlist("127.0.0.1")])
    {:ok, ipv4} = :inet.parse_ipv4_address(to_charlist("127.0.0.1"))
    :erl_boot_server.add_slave(ipv4)
    :ok
  end

  def create_peer(prefix) do
    ensure_distribution()
    current_cookie = :erlang.get_cookie()
    name = :peer.random_name(prefix)

    # start up the node
    {:ok, _pid, name} =
      :peer.start(%{
        name: name,
        longnames: true,
        host: ~c"127.0.0.1",
        args: [~c"-setcookie", to_charlist(current_cookie)]
      })

    {:ok, name}

    ################################################################################
    # load the code paths

    :ok = :rpc.block_call(name, :code, :add_paths, [:code.get_path()])
    {:ok, _} = :rpc.block_call(name, Application, :ensure_all_started, [:mix])
    :ok = :rpc.block_call(name, Mix, :env, [Mix.env()])

    # load all the applications
    for {app_name, _, _} <- Application.loaded_applications() do
      for {key, val} <- Application.get_all_env(app_name) do
        :ok = :rpc.block_call(name, Application, :put_env, [app_name, key, val])
      end
    end

    for {app_name, _, _} <- Application.loaded_applications() do
      Logger.debug "loading #{app_name}"
         :ok = :rpc.block_call(name, Application, :ensure_loaded, [app_name])
    end

    ################################################################################
    # provide config

    # I generate an automatic configuration for this peer based on my own configuration.
    # I compute random ports that dont collide with my ports or any other node's ports.
    peer_count = Enum.count(Node.list())

    # node config
    anoma_node_config = [
      anoma_node: [
        grpc_port: 50051 + peer_count + 100,
        mnesia: [rocksdb: false, persist_to_disk: false]
      ]
    ]

    :ok = :rpc.block_call(name, Application, :put_all_env, [anoma_node_config])

    # client config

    anoma_client_config = [
      anoma_client: [
        {Anoma.Client.Web.Endpoint,
         [
           server: true,
           adapter: Bandit.PhoenixAdapter,
           http: [ip: {127, 0, 0, 1}, port: 4000 + peer_count + 100],
           check_origin: false,
           debug_errors: false,
           render_errors: [view: Anoma.Client.Web.ErrorJSON, accepts: ["json"]],
           code_reloader: false
         ]},
        {:grpc_port, 4000 + peer_count + 100}
      ]
    ]

    :ok = :rpc.block_call(name, Application, :put_all_env, [anoma_client_config])

    # set the log level to warning
    :ok = :rpc.block_call(name, Application, :put_all_env, [[logger: [level: :warning]]])

    ################################################################################
    # read out config

    anoma_node_config = :rpc.block_call(name, Application, :get_all_env, [:anoma_node])
    anoma_client_config = :rpc.block_call(name, Application, :get_all_env, [:anoma_client])

    %{name: name, node_config: anoma_node_config, client_config: anoma_client_config}
  end

  def create_node(node_id \\ "deadbeef") do
    peer = create_peer(~c"node")

    # start the anoma_node application
    {:ok, _apps} = :rpc.block_call(peer.name, Application, :ensure_all_started, [:anoma_node])

    # start the node
    node_config = %{
      node_id: node_id,
      grpc_port: peer.node_config[:grpc_port],
      grpc_host: "localhost"
    }

    :rpc.block_call(peer.name, Anoma.Node.Examples.ENode, :start_node, [
      [node_id: node_id, node_config: node_config]
    ])

        # set the log level to error
    :ok = :rpc.block_call(peer.name, Logger, :configure, [[level: :debug]])

    Map.put(peer, :node_id, node_id)
  end

  @doc """
  I create a peer that will act as a node.
  """
  def create_client(node) do
    peer = create_peer(~c"client")

    # start the client
    {:ok, _apps} = :rpc.block_call(peer.name, Application, :ensure_all_started, [:anoma_client])

    # set the log level to error
    :ok = :rpc.block_call(peer.name, Logger, :configure, [[level: :debug]])

    # the grpc port on which the peer is listening
    node_grpc_port = node.node_config[:grpc_port]
    node_node_id = node.node_id

    :rpc.block_call(peer.name, Anoma.Client, :connect, ["localhost", node_grpc_port, node_node_id])

    peer
  end

  def stop_peer(name) do
    :peer.stop(name)
  end
end
