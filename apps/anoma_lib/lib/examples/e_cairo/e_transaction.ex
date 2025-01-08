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

  @spec a_shielded_transaction_with_intents() :: Transaction.t()
  def a_shielded_transaction_with_intents do
    action = EAction.an_action_with_intents()
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

  @spec a_shielded_transaction_with_multiple_actions() :: Transaction.t()
  def a_shielded_transaction_with_multiple_actions do
    an_action = EAction.an_action()
    another_action = EAction.an_action_with_intents()
    priv_keys = <<3::256>> <> <<3::256>>

    shielded_tx =
      %Transaction{
        actions: [an_action, another_action],
        delta: priv_keys
      }
      |> Transaction.finalize()

    assert Anoma.RM.Transaction.verify(shielded_tx)

    shielded_tx
  end

  @spec a_shielded_transaction_with_multiple_compliance_units() ::
          Transaction.t()
  def a_shielded_transaction_with_multiple_compliance_units do
    an_action = EAction.an_action_with_multiple_compliance_units()
    priv_keys = <<6::256>>

    shielded_tx =
      %Transaction{
        actions: [an_action],
        delta: priv_keys
      }
      |> Transaction.finalize()

    assert Anoma.RM.Transaction.verify(shielded_tx)

    shielded_tx
  end

  @spec a_shielded_transaction_from_compliance_units() ::
          Transaction.t()
  def a_shielded_transaction_from_compliance_units do
    compliance1_inputs =
      File.read!(
        Path.join(
          :code.priv_dir(:anoma_lib),
          "params/compliance1_inputs.json"
        )
      )

    compliance2_inputs =
      File.read!(
        Path.join(
          :code.priv_dir(:anoma_lib),
          "params/compliance2_inputs.json"
        )
      )

    resource_logic =
      File.read!(
        Path.join(
          :code.priv_dir(:anoma_lib),
          "params/trivial_resource_logic.json"
        )
      )

    witness =
      File.read!(
        Path.join(:code.priv_dir(:anoma_lib), "params/default_witness.json")
      )

    compliance_units = [compliance1_inputs, compliance2_inputs]
    input_logics = [resource_logic, resource_logic]
    input_witnesses = [witness, witness]
    output_logics = [resource_logic, resource_logic]
    output_witnesses = [witness, witness]

    {:ok, pre_tx} =
      Transaction.create_from_compliance_units(
        compliance_units,
        input_logics,
        input_witnesses,
        output_logics,
        output_witnesses
      )

    shielded_tx = Transaction.finalize(pre_tx)

    assert Anoma.RM.Transaction.verify(shielded_tx)

    shielded_tx
  end

  @spec a_shielded_transaction_composition() :: Transaction.t()
  def a_shielded_transaction_composition do
    priv_keys = <<3::256>>

    shielded_tx_1 =
      %Transaction{
        actions: [EAction.an_action()],
        delta: priv_keys
      }

    shielded_tx_2 =
      %Transaction{
        actions: [EAction.an_action_with_intents()],
        delta: priv_keys
      }

    shielded_tx =
      Anoma.RM.Transaction.compose(shielded_tx_1, shielded_tx_2)
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
