defmodule TestHelper.Executor do
  @moduledoc """
  I am a testing module allowing to listen to new spawned Executor workers.
  """

  require ExUnit.Assertions
  import ExUnit.Assertions

  @doc """
  I am the helper function for awaiting spawned Workers by the executor.

  I ask to fire a new transaction and then wait for the appropriate
  message. Upon message reception I give back the spawned Engine.
  """

  def wait_for_spawn(executor, id, tx_code, env \\ nil, timeout \\ nil) do
    Anoma.Node.Executor.fire_new_transaction(executor, id, tx_code, env)
    assert_receive({:"$gen_cast", {_, _, {:worker_spawned, var}}}, timeout)
    var
  end
end
