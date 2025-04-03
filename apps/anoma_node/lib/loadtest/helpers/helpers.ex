defmodule Helpers do
  @doc """
  Return the time in microseconds (μs)
  """
  @spec measure((-> term())) :: {non_neg_integer(), term()}
  def measure(proc) do
    :timer.tc(proc)
  end

  @doc """
  Run a function n times and report the average execution times and each result.
  """
  @spec average_measure((-> term()), non_neg_integer()) :: {non_neg_integer(), [term()]}
  def average_measure(proc, n) do
    {time, res} =
      for _ <- 1..n do
        measure(proc)
      end
      |> Enum.reduce({0, []}, fn {time, res}, {acc_time, acc_res} ->
        {time + acc_time, [res | acc_res]}
      end)

    {time / n, res}
  end

  @doc """
  Execute the function n times and return the results.
  """
  @spec do_n_times((-> term()), non_neg_integer()) :: [term()]
  def do_n_times(proc, n) do
    for _ <- 1..n do
      proc.()
    end
  end

end
