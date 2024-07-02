defmodule AnomaTest.Storage do
  use TestHelper.TestMacro, async: true

  alias Examples.ENode.EStorage

  doctest(Anoma.Node.Storage)

  test "examples" do
    EStorage.empty_storage()
    EStorage.put_storage()
    EStorage.snapshot_then_put()
    EStorage.snapshot_again()
    EStorage.deleting_the_put()
    EStorage.deleting_nothing_works_fine()
    EStorage.blocking_for_put()
    EStorage.august_node()
    EStorage.reserved_august()
    EStorage.august_node_proper()
    EStorage.londo_speaks_for_alice()
    EStorage.bertha_speaks_for_all()
  end
end
