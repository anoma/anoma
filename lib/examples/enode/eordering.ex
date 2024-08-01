defmodule Examples.ENode.EOrdering do
  require ExUnit.Assertions
  import ExUnit.Assertions

  alias Anoma.Symbol
  alias Anoma.Node
  alias Node.Ordering
  alias Examples.EOTransaction

  alias Examples.ENode.EStorage

  @doc """
  I am a miki_first test.

  I put an actual miki transaction into a new order. I check that the order
  was added to the storage succesfully and new order is updated properly.
  """

  @spec miki_first(Symbol.s()) :: Node.t()
  def miki_first(storage_name \\ "miki_first") do
    anode = EStorage.august_node(storage_name)
    first_transaction = EOTransaction.first_miki_transaction()
    assert [] == Ordering.all_orders(anode.ordering)
    assert 1 == Ordering.next_order(anode.ordering)

    Ordering.new_order(anode.ordering, [first_transaction])

    assert [{EStorage.random_id(), 1}] == Ordering.all_orders(anode.ordering)

    assert 2 == Ordering.next_order(anode.ordering)

    anode
  end

  @doc """
  I check that the ordering number is unique.

  Given the same miki_transaction with different order and same ID, I check
  that the ID is updated uniquely while the new_order is updated as if a
  new transaction got added.
  """

  @spec miki_also_second(Symbol.s()) :: Node.t()
  def miki_also_second(storage_name \\ "miki_also_second") do
    anode = miki_first(storage_name)
    second_transaction = EOTransaction.second_miki_transaction()

    Ordering.new_order(anode.ordering, [
      second_transaction
    ])

    assert [
             {EStorage.random_id(), 2}
           ] == Ordering.all_orders(anode.ordering)

    assert 3 == Ordering.next_order(anode.ordering)

    anode
  end

  @doc """
  I check that the Ordering info gets cleared on reset.

  I reset the storage and check that all orders and next order got reset.
  """

  @spec miki_clear(Symbol.s()) :: Node.t()
  def miki_clear(storage_name \\ "clear") do
    anode = miki_first(storage_name)

    Ordering.reset(anode.ordering)

    assert [] == Ordering.all_orders(anode.ordering)
    assert 1 == Ordering.next_order(anode.ordering)

    anode
  end
end
