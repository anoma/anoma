defmodule Anoma.Resource.Logic do
  @moduledoc false

  # intended behavior: must nullify a counter and commit to a counter
  # with a greater quantity.
  def counter_logic do
    # todo: far from real logic. just for testing
    [
      [0 | 50],
      [[0, 0 | 0] | 0] |
      Nock.stdlib_core()
    ]
  end
end
