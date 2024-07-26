defmodule Examples.ENode.EOredering do
  require ExUnit.Assertions
  import ExUnit.Assertions

  alias Anoma.Node.Ordering
  alias Examples.EOTransaction
  alias Anoma.Identity.Backend.Memory
  alias Anoma.Identity.{Manager, Name, Evidence, SignsFor, Verification}
  alias Anoma.Crypto.Id
  alias Anoma.Symbol
  alias Anoma.Node
  alias Anoma.Node.{Storage, Router, Identity.Commitment}

  alias Examples.{ENock, ENode, EIdentity, ECrypto}
  alias Examples.ENode.EStorage

  def miki_first(storage_name \\ "miki_first") do
    anode = EStorage.august_node(storage_name)
    first_transaction = EOTransaction.first_miki_transaction()
    assert [] == Ordering.all_orders(anode.ordering)
    assert 1 == Ordering.next_order(anode.ordering)

    Ordering.reset(anode.ordering)
    assert [] == Ordering.all_orders(anode.ordering)
    Ordering.new_order(anode.ordering, [first_transaction])

    assert [{EStorage.random_id(), 1}] == Ordering.all_orders(anode.ordering)

    assert 2 == Ordering.next_order(anode.ordering)

    anode
  end

  def miki_also_second(storage_name \\ "miki_also_second") do
    anode = miki_first(storage_name)
    first_transaction = EOTransaction.first_miki_transaction()

    Ordering.reset(anode.ordering)
    assert [] == Ordering.all_orders(anode.ordering)

    Ordering.new_order(anode.ordering, [first_transaction])
    assert [{EStorage.random_id(), 1}] == Ordering.all_orders(anode.ordering)

    anode
  end
end
