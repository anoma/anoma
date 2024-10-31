defmodule Examples.ERM.EShielded.ETransaction do
  alias Anoma.RM
  alias Examples.ERM.EShielded.EPartialTransaction

  use TestHelper.TestMacro

  @spec a_shielded_transaction() :: %RM.Shielded.Transaction{}
  def a_shielded_transaction do
    ptx = EPartialTransaction.a_partial_transaction()
    priv_keys = <<3::256>>

    shielded_tx =
      %RM.Shielded.Transaction{
        partial_transactions: [ptx],
        delta: priv_keys
      }
      |> RM.Shielded.Transaction.finalize()

    assert Anoma.RM.Transaction.verify(shielded_tx)

    shielded_tx
  end

  @spec duplicate_nfs_shielded_transaction() :: RM.Shielded.Transaction.t()
  def duplicate_nfs_shielded_transaction do
    ptx = EPartialTransaction.a_partial_transaction()
    priv_keys = <<3::256>>

    shielded_tx_1 =
      %RM.Shielded.Transaction{
        partial_transactions: [ptx],
        delta: priv_keys
      }

    shielded_tx_2 =
      %RM.Shielded.Transaction{
        partial_transactions: [ptx],
        delta: priv_keys
      }

    composed_shielded_tx =
      RM.Transaction.compose(shielded_tx_1, shielded_tx_2)
      |> RM.Shielded.Transaction.finalize()

    assert false == Anoma.RM.Transaction.verify(composed_shielded_tx)

    composed_shielded_tx
  end

  @spec a_invalid_shielded_transaction() :: %RM.Shielded.Transaction{}
  def a_invalid_shielded_transaction do
    ptx = EPartialTransaction.a_partial_transaction()
    # Use an invalid private key (all zeros) to create an invalid transaction
    invalid_priv_keys = <<0::256>>

    invalid_shielded_tx =
      %RM.Shielded.Transaction{
        partial_transactions: [ptx],
        delta: invalid_priv_keys
      }
      |> RM.Shielded.Transaction.finalize()

    assert false == Anoma.RM.Transaction.verify(invalid_shielded_tx)

    invalid_shielded_tx
  end
end
