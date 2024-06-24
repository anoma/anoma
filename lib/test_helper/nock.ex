defmodule TestHelper.Nock do
  @moduledoc """
  I am a testing module that has some common definitions for nock
  functions.
  """

  import Nock

  # TODO :: Add hoon code for this
  @spec increment_counter_val(Noun.t()) :: Noun.t()
  def increment_counter_val(val) do
    arm = [[1 | val], 4, 12, [1 | 0], [0 | 6], 1, val | 0]
    sample = 0
    [[8, [1 | sample], [1 | arm], 0 | 1] | logics_core()]
  end

  # [%ctr 0]
  def zero_counter(val) do
    arm = [1, val | 0]
    sample = 0
    [[8, [1 | sample], [1 | arm], 0 | 1] | logics_core()]
  end
end
