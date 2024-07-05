defmodule Examples.ENode.EClock do
  use Memoize

  require ExUnit.Assertions
  import ExUnit.Assertions

  alias Anoma.Node
  alias Anoma.Node.Clock
  alias Examples.ENode

  @spec start_clock() :: Node.t()
  def start_clock() do
    anode = ENode.zero_clock()
    assert 0 == Clock.get_epoch(anode.clock)
    anode
  end
end
