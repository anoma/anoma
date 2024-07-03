defmodule TestHelper.Mempool do
  require ExUnit.Assertions
  import ExUnit.Assertions

  def wait_for_tx(mempool, tx_code, timeout \\ nil) do
    Anoma.Node.Mempool.tx(mempool, tx_code)
    assert_receive({:"$gen_cast", {_, _, {:submitted, var}}}, timeout)
    var
  end
end
