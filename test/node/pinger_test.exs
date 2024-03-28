defmodule AnomaTest.Node.Pinger do
  use ExUnit.Case, async: true

  alias Anoma.Node.Mempool
  alias Anoma.Node.Router
  import TestHelper.Nock

  setup_all do
    storage = %Anoma.Storage{
      qualified: AnomaTest.Pinger.Qualified,
      order: AnomaTest.Pinger.Order
    }

    name = :pinger
    snapshot_path = [:my_special_nock_snaphsot | 0]

    {:ok, nodes} =
      Anoma.Node.start_link(
        new_storage: true,
        name: name,
        settings:
          [
            snapshot_path: snapshot_path,
            storage: storage,
            block_storage: :pinger_blocks,
            ping_time: 1
          ]
          |> Anoma.Node.start_min()
      )

    node = Anoma.Node.state(nodes)

    [node: node]
  end

  test "Execution is done automatically", %{node: node} do
    key = 555
    zero = zero_counter(key)

    assert :ok =
             Router.call(
               node.router,
               {:subscribe_topic, node.executor_topic.id, :local}
             )

    pid_zero = Mempool.tx(node.mempool, {:kv, zero}).pid

    assert_receive {:"$gen_cast", {_, {:process_done, ^pid_zero}}}
  end
end
