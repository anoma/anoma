defmodule Anoma.Node.Examples.ENarwhal.EMultiVM do
  @moduledoc """
  I contain cross-VM Narwhal consensus examples.

  Each example is a thin wrapper over the in-VM `ENarwhal.EConsensus`
  example of the same name, run with a *distributed* config: validator
  0 stays on this VM, the rest are placed on random free ports so
  `ECluster.setup/1` spins them up in fresh peer VMs and wires the advert
  mesh. The consensus logic itself is identical; only the config differs.

  Peer VMs are not torn down; they die with this BEAM, exactly as the
  in-VM examples leak their nodes. Run with a named BEAM (`--sname`).
  """

  alias Anoma.Node.Examples.ENarwhal
  alias Anoma.Node.Examples.ENarwhal.ECluster
  alias Anoma.Node.Examples.ENarwhal.EConsensus
  alias Anoma.Node.Transaction.Narwhal.Config

  @doc "I verify a multi-VM validator set comes up, connects, and tears down."
  @spec discovery_only([Config.t()], boolean()) :: [Config.t()]
  def discovery_only(configs \\ distributed_configs(), teardown? \\ true) do
    ECluster.with_validators(configs, fn cfgs -> cfgs end, teardown?)
  end

  @doc "I verify Bullshark commits a transaction across VMs."
  @spec transaction_committed([Config.t()], boolean()) :: [Config.t()]
  def transaction_committed(
        configs \\ distributed_configs(),
        teardown? \\ true
      ) do
    EConsensus.transaction_committed(configs, teardown?)
  end

  @doc "I verify a validator re-fetches wiped blocks over the wire."
  @spec recovery_after_block_wipe([Config.t()], boolean()) :: [Config.t()]
  def recovery_after_block_wipe(
        configs \\ distributed_configs(),
        teardown? \\ true
      ) do
    EConsensus.recovery_after_block_wipe(configs, teardown?)
  end

  # Validator 0 on this VM; the rest on random free ports so each run
  # gets fresh ports and leaked peers never collide.
  @spec distributed_configs(pos_integer()) :: [Config.t()]
  defp distributed_configs(count \\ 4) do
    my_port = Application.get_env(:anoma_node, :grpc_port)

    ENarwhal.with_ports(Config.generate_configs(count), fn
      0 -> my_port
      _ -> free_port()
    end)
  end

  @spec free_port() :: pos_integer()
  defp free_port do
    {:ok, socket} = :gen_tcp.listen(0, [])
    {:ok, port} = :inet.port(socket)
    :gen_tcp.close(socket)
    port
  end
end
