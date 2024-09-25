defmodule TransactionTest do
  use ExUnit.Case, async: true

  alias Anoma.Node.Examples.ETransaction

  test "storage examples" do
    ETransaction.write_then_read()
    ETransaction.write_then_read_other()
    ETransaction.read_future_then_write()
    ETransaction.read_future_then_write()
    ETransaction.read_other_future_then_write()
    ETransaction.write_future_then_write_present()
    ETransaction.write_multiple_then_read()
    ETransaction.write_future_multiple_then_write_present()
    ETransaction.complicated_storage()
    ETransaction.complicated_storage_with_commit()
  end

  test "ordering examples" do
    ETransaction.ord_write_then_read()
    ETransaction.ord_read_future_then_write()
    ETransaction.ord_order_first()
  end

  test "transaction examples" do
    ETransaction.zero_counter_submit()
    ETransaction.inc_counter_submit_with_zero()
    ETransaction.inc_counter_submit_after_zero()
    ETransaction.inc_counter_submit_after_read()
    ETransaction.bluf_transaction_errors()
    ETransaction.read_txs_write_nothing()
    ETransaction.bluff_txs_write_nothing()
  end
end
