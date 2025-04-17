defmodule Anoma.Node.Examples.EAdvertise do
  alias Anoma.Node.Examples.ENode
  alias Anoma.Node.Transport.GRPC.Advertise
  alias Anoma.Node.Transport.NetworkRegister
  alias Anoma.Node.Transport.NetworkRegister.Advert
  alias Anoma.Node.Transport.NetworkRegister.Advert.GRPCAddress

  import ExUnit.Assertions
  import Anoma.Examples.Helpers

  require Logger

  use TypedStruct

  ############################################################
  #                    Context                               #
  ############################################################

  typedstruct module: Peer do
    @typedoc """
    I am a peer node.

    ### Fields
    - `:name`       - The name of the remote node.
    - `:server_ref` - The server ref pid of this node.
    """
    field(:name, atom())
    field(:server_ref, pid())
  end

  ############################################################
  #                  Public API                              #
  ############################################################

  # @doc """
  # I create a config for a node based on its node id.
  #
  # Note here that the GRPC port is the same for both nodes. The GRPC endpoint
  # is not multi-homed, so all nodes listen on the same GRPC port.
  # """
  @spec node_config(map()) :: map()
  defp node_config(params \\ %{}) do
    %{
      grpc_port: Application.get_env(:anoma_node, :grpc_port),
      grpc_host: "localhost",
      seed_nodes: %{},
      node_id: Base.encode16(:crypto.strong_rand_bytes(32))
    }
    |> Map.merge(params)
  end

  defp seed_node_entry_for(node_config) do
    %Advert{
      node_id: node_config.node_id,
      grpc_address: %GRPCAddress{
        host: node_config.grpc_host,
        port: node_config.grpc_port
      },
      version: "unknown"
    }
  end

  @doc """
  I create two nodes, and I advertise the first node to the second node. I do
  this by sending a GRPC request that advertises the first node to the second
  node.
  """
  @spec advertise_node :: {map(), map()}
  def advertise_node() do
    # node 1
    cfg1 = node_config()
    node_1 = ENode.start_node(node_config: cfg1, node_id: cfg1.node_id)

    # node 2
    cfg2 = node_config()
    node_2 = ENode.start_node(node_id: cfg2.node_id, node_config: cfg2)

    # manually advertise node 1 to node 2 via GRPC
    #
    # note that this is a workaround because usually nodes do not explicitly
    # advertise via grpc. Here I call the GRPC endpoint directly.
    #
    # this address is the same for both nodes since they run in the same VM.
    grpc_port = Application.get_env(:anoma_node, :grpc_port)
    address = %GRPCAddress{host: "localhost", port: grpc_port}

    Advertise.advertise(cfg1, cfg2.node_id, %Advert{
      node_id: cfg1.node_id,
      grpc_address: address,
      version: "unknown"
    })

    # assert that node 2's network register know node 1.
    node_2_register = NetworkRegister.dump_register(node_2.node_id)
    assert Map.get(node_2_register.nodes, node_1.node_id) != nil

    # since node 2 knows node 1, it can advertise directly without manually crafting the message.
    NetworkRegister.advertise_to(node_2.node_id, node_1.node_id)

    # return the nodes
    {node_1, node_2}
  end

  @doc """
  I test advertisement using a distributed node.

  I work but i break other tests, so I'm disabled in apps/anoma_node/test/advertise_test.exs
  """
  @spec seed_nodes_distributed(Keyword.t()) :: {map(), map(), Peer.t()}
  def seed_nodes_distributed(opts \\ [stop_slave: true]) do
    cfg_local = node_config()

    ENode.start_node(node_id: cfg_local.node_id, node_config: cfg_local)

    # create a seed node entry for the local node
    local_info = seed_node_entry_for(cfg_local)

    # start a node in a separate vm
    cfg_remote =
      node_config(%{seed_nodes: %{cfg_local.node_id => local_info}})
      # the grpc port of the slave is ours + 1 (see start_slave())
      |> Map.update!(:grpc_port, &(&1 + 1500))

    # start a second vm
    peer = start_slave()

    # start a node on the second vm
    args = [node_config: cfg_remote, node_id: cfg_remote.node_id]
    :rpc.block_call(peer.name, ENode, :start_node, [args])

    # assert that the local node knows about the remote node
    assert_eventually(fn ->
      local_register = NetworkRegister.dump_register(cfg_local.node_id)

      # check that the node is known by the other node
      assert Map.get(local_register.nodes, cfg_remote.node_id) != nil

      # check that the address is correct
      {advert, _timestamp} = Map.get(local_register.nodes, cfg_remote.node_id)
      assert advert.grpc_address.host == cfg_remote.grpc_host
      assert advert.grpc_address.port == cfg_remote.grpc_port
    end)

    # assert that the other node knows this node
    assert_eventually(fn ->
      remote_register =
        :rpc.block_call(peer.name, NetworkRegister, :dump_register, [
          cfg_remote.node_id
        ])

      # check that the remote_register is known by the other node
      assert Map.get(remote_register.nodes, cfg_local.node_id) != nil

      # check that the address is correct
      {advert, _timestamp} = Map.get(remote_register.nodes, cfg_local.node_id)
      assert advert.grpc_address.host == cfg_local.grpc_host
      assert advert.grpc_address.port == cfg_local.grpc_port
    end)

    if Keyword.get(opts, :stop_slave, true) do
      stop_slave(peer)
    end

    # return the nodes
    {cfg_local, cfg_remote, peer}
  end

  @doc """
  I stop the slave from running.

  Stopping the slave means disabling distribution, stopping the net kernel, and
  restarting mnesia.
  """
  @spec stop_slave(Peer.t()) :: :ok
  def stop_slave(%Peer{} = peer) do
    # delete the slave from the system
    {:ok, ipv4} = :inet.parse_ipv4_address(to_charlist("127.0.0.1"))
    :erl_boot_server.delete_slave(ipv4)

    # stop the slave vm
    :peer.stop(peer.server_ref)

    # stop distribution
    :net_kernel.stop()

    # restart mnesia (it will break because distribution is disabled above)
    :mnesia.stop()
    :mnesia.start()

    :ok
  end

  # @doc """
  # I start a second erlang vm as a slave to test distributed pub sub.
  # """
  @spec start_slave(charlist()) :: Peer.t()
  defp start_slave(name \\ :peer.random_name()) do
    # if this is a test run, the node will not be distributed, so start that here
    :ok =
      case :net_kernel.start([:"primary@127.0.0.1"]) do
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

    :erl_boot_server.start([])
    {:ok, ipv4} = :inet.parse_ipv4_address(to_charlist("127.0.0.1"))
    :erl_boot_server.add_slave(ipv4)

    my_hostname =
      node()
      |> Atom.to_string()
      |> String.split("@")
      |> Enum.at(1)
      |> to_charlist()

    # the cookie in use by this beam
    current_cookie = :erlang.get_cookie()

    # start up the node
    {:ok, pid, node} =
      :peer.start(%{
        name: name,
        longnames: true,
        host: my_hostname,
        args: [~c"-setcookie", to_charlist(current_cookie)]
      })

    peer_node = %Peer{name: node, server_ref: pid}
    # add my code path to the slave
    :rpc.block_call(peer_node.name, :code, :add_paths, [:code.get_path()])

    # copy over our configuration to the remote node
    for {app_name, _, _} <- Application.loaded_applications() do
      for {key, val} <- Application.get_all_env(app_name) do
        :rpc.block_call(peer_node.name, Application, :put_env, [
          app_name,
          key,
          val
        ])
      end
    end

    # set the grpc ports and http ports of the remote node to be 1 higher than ours.
    my_grpc_port = Application.get_env(:anoma_node, :grpc_port)

    :rpc.block_call(peer_node.name, Application, :put_all_env, [
      [
        anoma_node: [grpc_port: my_grpc_port + 1500],
        anoma_client: [
          {:grpc_port, 40052},
          {Anoma.Client.Web.Endpoint, [http: [port: 4001]]},
          {Anoma.Client.Web.SocketHandler, [port: 3001]}
        ]
      ]
    ])

    #
    # ensure mix is started
    :rpc.block_call(peer_node.name, Application, :ensure_all_started, [:mix])

    # ensure all other applications are started as well
    :rpc.block_call(peer_node.name, Mix, :env, [:prod])

    for {app_name, _, _} <- Application.loaded_applications() do
      :rpc.block_call(peer_node.name, Application, :ensure_all_started, [
        app_name
      ])
    end

    peer_node
  end
end
