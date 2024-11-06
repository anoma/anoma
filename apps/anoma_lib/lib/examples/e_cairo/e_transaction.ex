defmodule Examples.ECairo.ETransaction do
  alias Anoma.CairoResource.Transaction
  alias Examples.ECairo.EAction

  use TestHelper.TestMacro

  @spec a_shielded_transaction() :: Transaction.t()
  def a_shielded_transaction do
    action = EAction.an_action()
    priv_keys = <<3::256>>

    shielded_tx =
      %Transaction{
        actions: [action],
        delta: priv_keys
      }
      |> Transaction.finalize()

    assert Anoma.RM.Transaction.verify(shielded_tx)

    shielded_tx
  end

  @spec duplicate_nfs_shielded_transaction() :: Transaction.t()
  def duplicate_nfs_shielded_transaction do
    action = EAction.an_action()
    priv_keys = <<3::256>>

    shielded_tx_1 =
      %Transaction{
        actions: [action],
        delta: priv_keys
      }

    shielded_tx_2 =
      %Transaction{
        actions: [action],
        delta: priv_keys
      }

    composed_shielded_tx =
      Anoma.RM.Transaction.compose(shielded_tx_1, shielded_tx_2)
      |> Transaction.finalize()

    assert false == Anoma.RM.Transaction.verify(composed_shielded_tx)

    composed_shielded_tx
  end

  @spec a_invalid_shielded_transaction() :: Transaction.t()
  def a_invalid_shielded_transaction do
    action = EAction.an_action()
    # Use an invalid private key (all zeros) to create an invalid transaction
    invalid_priv_keys = <<0::256>>

    invalid_shielded_tx =
      %Transaction{
        actions: [action],
        delta: invalid_priv_keys
      }
      |> Transaction.finalize()

    assert false == Anoma.RM.Transaction.verify(invalid_shielded_tx)

    invalid_shielded_tx
  end
end
