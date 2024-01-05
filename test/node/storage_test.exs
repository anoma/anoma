defmodule AnomaTest.Node.Storage do
  use ExUnit.Case

  alias Anoma.Order
  alias Anoma.Node.Storage.{Communicator, Ordering}

  doctest(Anoma.Node.Storage)
  doctest(Anoma.Node.Storage.Communicator)
  doctest(Anoma.Node.Storage.Ordering)

  setup_all do
    # base storage testing default
    storage = %Anoma.Storage{
      qualified: AnomaTest.Node.Storage.Qualified,
      order: AnomaTest.Node.Storage.Order
    }

    ordering = :node_storage_com

    unless Process.whereis(ordering) do
      Anoma.Node.Storage.start_link(name: :node_storage, table: storage)
    end

    [ordering: ordering]
  end

  test "reset works", %{ordering: ordering} do
    Communicator.new_order(ordering, [Order.new(1, <<3>>, self())])
    Communicator.reset(ordering)
    ordering = Communicator.state(ordering)
    assert ordering.hash_to_order == %{}
    assert ordering.next_order == 1
  end

  test "added to the hash_ordering", %{ordering: ordering} do
    Communicator.reset(ordering)

    Communicator.new_order(
      ordering,
      [Order.new(1, <<3>>, self()), Order.new(1, <<3>>, self())],
      false
    )

    ordering = Communicator.state(ordering)
    assert ordering.hash_to_order == %{<<3>> => 1}
  end

  test "getting proper offsets", %{ordering: ordering} do
    Communicator.reset(ordering)
    assert 1 == Communicator.next_order(ordering)

    Communicator.new_order(ordering, [
      Order.new(1, <<3>>, self()),
      Order.new(2, <<3>>, self())
    ])

    assert 3 == Communicator.next_order(ordering)
  end

  test "receiving read readies", %{ordering: ordering} do
    Communicator.new_order(ordering, [
      Order.new(3, <<3>>, self()),
      Order.new(4, <<3>>, self())
    ])

    assert_receive {:read_ready, 3}
    assert_receive {:read_ready, 4}
  end

  test "we properly cache orders", %{ordering: ordering} do
    Communicator.reset(ordering)
    assert Communicator.true_order(ordering, <<3>>) == nil
    Communicator.new_order(ordering, [Order.new(1, <<3>>, self())])
    assert Communicator.true_order(ordering, <<3>>) == 1
  end
end
