defmodule Examples.ERM.EShielded.ETransaction do
  alias Anoma.RM
  alias Examples.ERM.EShielded.EPartialTransaction

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

    shielded_tx
  end

  @spec another_shielded_transaction() :: %RM.Shielded.Transaction{}
  def another_shielded_transaction do
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

    invalid_shielded_tx
  end
end
