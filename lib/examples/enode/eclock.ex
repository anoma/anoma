defmodule Examples.ENode.EClock do
  use Memoize

  require ExUnit.Assertions
  import ExUnit.Assertions

  alias Anoma.Node.Router.Engine
  alias Anoma.Node
  alias Examples.ENode

  @spec start_clock() :: Node.t()
  def start_clock() do
    anode = ENode.zero_clock()
    assert 0 == Engine.get_state(anode.clock).start
    anode
  end
end
