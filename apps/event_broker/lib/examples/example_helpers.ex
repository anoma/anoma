defmodule Anoma.Examples.Helpers do
  require ExUnit.Assertions

  @doc """
  A test helper for asserting that a function will return
  a truthy value eventually within a given time frame.
  """
  @spec assert_eventually((-> any()), non_neg_integer(), non_neg_integer()) ::
          any()
  def assert_eventually(fun, timeout \\ 100, interval \\ 10)

  def assert_eventually(_fun, timeout, _interval) when timeout <= 0 do
    raise ExUnit.AssertionError,
          "Eventually assertion failed to receive a truthy result before timeout."
  end

  def assert_eventually(fun, timeout, interval) do
    result = fun.()
    ExUnit.Assertions.assert(result)
    result
  rescue
    ExUnit.AssertionError ->
      Process.sleep(interval)
      assert_eventually(fun, timeout - interval, interval)
  end
end
