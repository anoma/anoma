defmodule Anoma.SupervisorTest do
  use ExUnit.Case, async: true

  setup do
    Application.put_env(:anoma_node, :grpc_port, 0)
    {:ok, sup} = Anoma.Supervisor.start_link([])
    on_exit(fn -> Supervisor.stop(sup) end)
    :ok
  end

  test "stop_node returns :ok when node is unknown" do
    assert :ok = Anoma.Supervisor.stop_node("unknown-node")
  end

  test "stop_node stops a running node (and is idempotent)" do
    node_id = Base.encode16(:crypto.strong_rand_bytes(8))

    {:ok, _pid} =
      Anoma.Supervisor.start_node(
        node_id: node_id,
        transaction: [mempool: []]
      )

    pid =
      GenServer.whereis(
        Anoma.Node.Registry.via(node_id, Anoma.Node.Supervisor)
      )

    assert is_pid(pid)

    ref = Process.monitor(pid)
    assert :ok = Anoma.Supervisor.stop_node(node_id)
    assert_receive {:DOWN, ^ref, :process, ^pid, _}, 5_000

    assert :ok = Anoma.Supervisor.stop_node(node_id)
  end
end
