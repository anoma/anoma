defmodule Examples.EShieldedTransaction do

  alias Anoma.ShieldedResource.ShieldedTransaction
  alias Anoma.RM.Transaction
  alias Examples.EShieldedPartialTransaction

  @spec a_shielded_transaction() :: %ShieldedTransaction{}
  def a_shielded_transaction do
    ptx = EShieldedPartialTransaction.a_partial_transaction()
    priv_keys = :binary.copy(<<0>>, 31) <> <<3>>

    shielded_tx =
      %ShieldedTransaction{
        partial_transactions: [ptx],
        delta: priv_keys
      }
      |> ShieldedTransaction.finalize()

    shielded_tx
  end

  @spec another_shielded_transaction() :: %ShieldedTransaction{}
  def another_shielded_transaction do
    ptx = EShieldedPartialTransaction.a_partial_transaction()
    priv_keys = :binary.copy(<<0>>, 31) <> <<3>>

    shielded_tx_1 =
      %ShieldedTransaction{
        partial_transactions: [ptx],
        delta: priv_keys
      }

    shielded_tx_2 =
      %ShieldedTransaction{
        partial_transactions: [ptx],
        delta: priv_keys
      }

    composed_shielded_tx = Transaction.compose(shielded_tx_1,shielded_tx_2)
     |> ShieldedTransaction.finalize()

    composed_shielded_tx
  end

  @spec a_invalid_shielded_transaction() :: %ShieldedTransaction{}
  def a_invalid_shielded_transaction do
    ptx = EShieldedPartialTransaction.a_partial_transaction()
    # Use an invalid private key (all zeros) to create an invalid transaction
    invalid_priv_keys = :binary.copy(<<0>>, 32)

    invalid_shielded_tx =
      %ShieldedTransaction{
        partial_transactions: [ptx],
        delta: invalid_priv_keys
      }
      |> ShieldedTransaction.finalize()

    invalid_shielded_tx
  end

end
