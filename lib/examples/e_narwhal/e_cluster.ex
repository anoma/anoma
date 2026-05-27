defmodule Anoma.Node.Examples.ENarwhal.ECluster do
  @moduledoc """
  I bring a Narwhal validator set up for examples and tear it down.

  Each config's `pk_to_address` port decides where its validator runs:
  on this VM when the port is our gRPC port, otherwise in a fresh peer
  VM. So one harness drives both in-VM (`local_configs/1`) and cross-VM
  (`ENarwhal.EMultiVM.distributed_configs/1`) runs; only the ports differ.

  `with_validators/3` is the entry point. `ENarwhal.EConsensus` scenarios
  and the `ENarwhal.EMultiVM` wrappers both sit on me.
  """

  alias Anoma.Node.Examples.ENarwhal
  alias Anoma.Node.Examples.EAdvertise
  alias Anoma.Node.Transport.GRPC.Advertise
  alias Anoma.Node.Transport.NetworkRegister.Advert
  alias Anoma.Node.Transport.NetworkRegister.Advert.GRPCAddress
  alias Anoma.Node.Transport.Proxy
  alias Anoma.Node.Registry
  alias Anoma.Node.Transaction.Mempool
  alias Anoma.Node.Transaction.Narwhal.Config

  import Anoma.Examples.Helpers, only: [assert_eventually: 3]

  @doc """
  I set up `configs`, run `scenario` on them, and tear them down unless
  `teardown?` is false. Pass false to compose another scenario on the
  live set, or to poke at them in a session. I return the scenario's
  result.
  """
  @spec with_validators(
          [Config.t()],
          ([Config.t()] -> [Config.t()]),
          boolean()
        ) :: [Config.t()]
  def with_validators(configs, scenario, teardown? \\ true) do
    validators = setup(configs)
    result = scenario.(configs)
    if teardown?, do: teardown(validators)
    result
  end

  @doc """
  I bring `configs` up as a connected validator set, returning each
  config paired with its peer-VM handle (`nil` if it runs on this VM).
  A validator runs locally when its advertised port is this VM's gRPC
  port, else in a fresh peer VM. All-local sets share the broker
  directly; otherwise I fan out an advert mesh until every validator
  holds a proxy for every peer.
  """
  @spec setup([Config.t()]) :: [{Config.t(), EAdvertise.Peer.t() | nil}]
  def setup(configs) do
    validators = Enum.map(configs, fn cfg -> {cfg, spin_up(cfg)} end)
    await_mempools(validators)
    if foreign?(validators), do: connect_mesh(validators)
    validators
  end

  @doc """
  I stop exactly the validators `setup/1` spun up: a peer VM (and its
  gRPC port) via `:peer.stop`, a local node in place. I touch neither
  mnesia nor distribution, so a `--sname` session survives.
  """
  @spec teardown([{Config.t(), EAdvertise.Peer.t() | nil}]) :: :ok
  def teardown(validators) do
    Enum.each(validators, fn
      {cfg, nil} ->
        stop_quietly(fn -> Anoma.Supervisor.stop_node(cfg.node_id) end)

      {_cfg, peer} ->
        stop_quietly(fn -> :peer.stop(peer.server_ref) end)
    end)

    :ok
  end

  @doc """
  In-VM configs: every validator advertises this VM's port, so `setup/1`
  runs them all locally (no peers, shared broker).
  """
  @spec local_configs(pos_integer()) :: [Config.t()]
  def local_configs(count \\ 4) do
    my_port = Application.get_env(:anoma_node, :grpc_port)
    ENarwhal.with_ports(Config.generate_configs(count), fn _i -> my_port end)
  end

  @spec foreign?([{Config.t(), EAdvertise.Peer.t() | nil}]) :: boolean()
  defp foreign?(validators) do
    Enum.any?(validators, fn {_cfg, peer} -> peer end)
  end

  @spec stop_quietly((-> any())) :: :ok
  defp stop_quietly(fun) do
    try do
      fun.()
    catch
      :exit, _ -> :ok
    end

    :ok
  end

  # Run `cfg` locally if its address is this VM's gRPC port, else in a
  # fresh peer VM. Returns the peer handle, or nil for the local one.
  @spec spin_up(Config.t()) :: EAdvertise.Peer.t() | nil
  defp spin_up(cfg) do
    my_port = Application.get_env(:anoma_node, :grpc_port)
    %{port: port} = Map.fetch!(cfg.pk_to_address, cfg.public_key)

    if port == my_port do
      ENarwhal.start_narwhal_node(cfg, batch_size: 1)
      nil
    else
      peer = EAdvertise.start_slave(:peer.random_name(), port - my_port)
      # Match the parent's Mix.env so the peer accepts our debug txs.
      :rpc.block_call(peer.name, Mix, :env, [Mix.env()])

      :rpc.block_call(peer.name, ENarwhal, :start_narwhal_node, [
        cfg,
        [batch_size: 1]
      ])

      peer
    end
  end

  @spec await_mempools([{Config.t(), EAdvertise.Peer.t() | nil}]) :: :ok
  defp await_mempools(validators) do
    for {cfg, peer} <- validators do
      assert_eventually(
        fn -> whereis(peer, cfg.node_id, Mempool) != nil end,
        30_000,
        200
      )
    end

    :ok
  end

  # Re-fire the advert fan-out until every validator holds a Proxy.Node
  # for every peer (proxy creation is idempotent, so retrying is safe).
  @spec connect_mesh([{Config.t(), EAdvertise.Peer.t() | nil}]) :: :ok
  defp connect_mesh(validators) do
    configs = Enum.map(validators, fn {cfg, _peer} -> cfg end)

    assert_eventually(
      fn ->
        advertise_all_pairs(configs)

        Enum.all?(validators, fn {cfg, peer} ->
          Enum.all?(configs, fn other ->
            other.node_id == cfg.node_id or
              whereis(peer, other.node_id, Proxy.Node) != nil
          end)
        end)
      end,
      60_000,
      500
    )
  end

  @spec advertise_all_pairs([Config.t()]) :: :ok
  defp advertise_all_pairs(configs) do
    for sender <- configs,
        receiver <- configs,
        sender.public_key != receiver.public_key do
      %{host: host, port: port} =
        Map.fetch!(sender.pk_to_address, sender.public_key)

      Advertise.advertise(
        %{node_id: sender.node_id, grpc_host: host, grpc_port: port},
        receiver.node_id,
        self_advert(receiver)
      )
    end

    :ok
  end

  @spec self_advert(Config.t()) :: Advert.t()
  defp self_advert(cfg) do
    %{host: host, port: port} = Map.fetch!(cfg.pk_to_address, cfg.public_key)

    %Advert{
      node_id: cfg.node_id,
      grpc_address: %GRPCAddress{host: host, port: port},
      version: "unknown"
    }
  end

  # Registry lookup on a validator's VM: locally if `nil`, else via :rpc.
  @spec whereis(EAdvertise.Peer.t() | nil, String.t(), module()) ::
          pid() | nil
  defp whereis(nil, node_id, engine), do: Registry.whereis(node_id, engine)

  defp whereis(peer, node_id, engine) do
    :rpc.block_call(peer.name, Registry, :whereis, [node_id, engine])
  end
end
