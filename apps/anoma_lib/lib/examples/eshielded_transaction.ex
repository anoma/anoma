defmodule Examples.EShieldedTransaction do
  alias Anoma.RM.Shielded.Transaction
  alias Anoma.RM
  alias Examples.EShieldedPartialTransaction

  @spec a_shielded_transaction() :: %Transaction{}
  def a_shielded_transaction do
    ptx = EShieldedPartialTransaction.a_partial_transaction()
    priv_keys = :binary.copy(<<0>>, 31) <> <<3>>

    shielded_tx =
      %Transaction{
        partial_transactions: [ptx],
        delta: priv_keys
      }
      |> Transaction.finalize()

    shielded_tx
  end

  @spec another_shielded_transaction() :: %Transaction{}
  def another_shielded_transaction do
    ptx = EShieldedPartialTransaction.a_partial_transaction()
    priv_keys = :binary.copy(<<0>>, 31) <> <<3>>

    shielded_tx_1 =
      %Transaction{
        partial_transactions: [ptx],
        delta: priv_keys
      }

    shielded_tx_2 =
      %Transaction{
        partial_transactions: [ptx],
        delta: priv_keys
      }

    composed_shielded_tx =
      RM.Transaction.compose(shielded_tx_1, shielded_tx_2)
      |> Transaction.finalize()

    composed_shielded_tx
  end

  @spec a_invalid_shielded_transaction() :: %Transaction{}
  def a_invalid_shielded_transaction do
    ptx = EShieldedPartialTransaction.a_partial_transaction()
    # Use an invalid private key (all zeros) to create an invalid transaction
    invalid_priv_keys = :binary.copy(<<0>>, 32)

    invalid_shielded_tx =
      %Transaction{
        partial_transactions: [ptx],
        delta: invalid_priv_keys
      }
      |> Transaction.finalize()

    invalid_shielded_tx
  end
end
