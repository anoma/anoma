defmodule TestHelper.Mempool do
  require ExUnit.Assertions
  import ExUnit.Assertions

  @doc """
  I am a helper function waiting for launching transaction code.

  I ask the mempool to use it's `tx/2` functionality with the given
  transaction code. I then await for a `:submitted` message in the mailbox
  for a timeout-specified ammount of time.

  Afterwards I return the pending transaction structure.
  """

  def wait_for_tx(mempool, tx_code, timeout \\ nil) do
    Anoma.Node.Mempool.tx(mempool, tx_code)
    assert_receive({:"$gen_cast", {_, _, {:submitted, var}}}, timeout)
    var
  end
end
