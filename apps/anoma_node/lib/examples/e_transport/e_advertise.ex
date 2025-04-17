defmodule Anoma.Node.Examples.EAdvertise do
  alias Anoma.Node.Examples.ENode
  alias Anoma.Node.Transport.GRPC.Advertise
  alias Anoma.Node.Transport.NetworkRegister
  alias Anoma.Node.Transport.NetworkRegister.Advert
  alias Anoma.Node.Transport.NetworkRegister.Advert.GRPCAddress

  import ExUnit.Assertions
  import Anoma.Examples.Helpers

  require Logger

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
  @spec seed_nodes_distributed(Keyword.t()) :: {map(), map(), atom()}
  def seed_nodes_distributed(opts \\ [stop_slave: true]) do
    cfg_local = node_config()

    ENode.start_node(node_id: cfg_local.node_id, node_config: cfg_local)

    # create a seed node entry for the local node
    local_info = seed_node_entry_for(cfg_local)

    # start a node in a separate vm
    cfg_remote =
      node_config(%{seed_nodes: %{cfg_local.node_id => local_info}})
      # the grpc port of the slave is ours + 1 (see start_slave())
      |> Map.update!(:grpc_port, &(&1 + 1))

    # start a second vm
    slave = start_slave()

    # start a node on the second vm
    args = [node_config: cfg_remote, node_id: cfg_remote.node_id]
    :rpc.block_call(slave, ENode, :start_node, [args])

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
        :rpc.block_call(slave, NetworkRegister, :dump_register, [
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
      stop_slave(slave)
    end

    # return the nodes
    {cfg_local, cfg_remote, slave}
  end

  @doc """
  I stop the slave from running.

  Stopping the slave means disabling distribution, stopping the net kernel, and
  restarting mnesia.
  """
  def stop_slave(name) do
    # delete the slave from the system
    {:ok, ipv4} = :inet.parse_ipv4_address(to_charlist("127.0.0.1"))
    :erl_boot_server.delete_slave(ipv4)

    # stop the slave vm
    :slave.stop(name)

    # stop distribution
    :net_kernel.stop()

    # restart mnesia (it will break because distribution is disabled above)
    :mnesia.stop()
    :mnesia.start()
  end

  # @doc """
  # I start a second erlang vm as a slave to test distributed pub sub.
  # """
  @spec start_slave(atom()) :: atom()
  defp start_slave(name \\ :slave) do
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

    {:ok, node} =
      case :slave.start(my_hostname, name) do
        {:ok, node} -> {:ok, node}
        {:error, {:already_running, node}} -> {:ok, node}
        err -> err
      end

    # add my code path to the slave
    :rpc.block_call(node, :code, :add_paths, [:code.get_path()])

    # copy over our configuration to the remote node
    for {app_name, _, _} <- Application.loaded_applications() do
      for {key, val} <- Application.get_all_env(app_name) do
        :rpc.block_call(node, Application, :put_env, [app_name, key, val])
      end
    end

    # set the grpc port to our port + 1
    my_grpc_port = Application.get_env(:anoma_node, :grpc_port)

    :rpc.block_call(node, Application, :put_env, [
      :anoma_node,
      :grpc_port,
      my_grpc_port + 1
    ])

    # ensure mix is started
    :rpc.block_call(node, Application, :ensure_all_started, [:mix])

    # ensure all other applications are started as well
    :rpc.block_call(node, Mix, :env, [:prod])

    for {app_name, _, _} <- Application.loaded_applications() do
      :rpc.block_call(node, Application, :ensure_all_started, [app_name])
    end

    node
  end
end
