defmodule TestHelper.Worker do
  import ExUnit.Assertions

  # I don't understand why dialyzer complains about this because it works and
  # is obviously correct
  @dialyzer {:nowarn_function, [wait_for_worker: 2]}
  def wait_for_worker(worker_addr, status \\ nil) do
    if status == nil do
      assert_receive {:"$gen_cast",
                      {:router_cast, _, {:worker_done, ^worker_addr, _}}},
                     5000
    else
      assert_receive {:"$gen_cast",
                      {:router_cast, _, {:worker_done, ^worker_addr, ^status}}},
                     5000
    end
  end

  def wait_for_read_value(value) do
    assert_receive {:"$gen_cast", {:router_cast, _, {:read_value, ^value}}},
                   5000
  end
end
