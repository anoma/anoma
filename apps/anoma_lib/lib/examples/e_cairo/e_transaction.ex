defmodule Examples.ECairo.ETransaction do
  use Memoize

  alias Anoma.CairoResource.{LogicInstance, Transaction}
  alias Examples.ECairo.EAction

  use TestHelper.TestMacro

  @spec a_shielded_transaction() :: Transaction.t()
  defmemo a_shielded_transaction do
    action = EAction.an_action()
    priv_keys = <<3::256>>

    shielded_tx =
      %Transaction{
        actions: MapSet.new([action]),
        delta_proof: priv_keys
      }
      |> Transaction.prove_delta()

    assert true == Transaction.verify(shielded_tx)

    shielded_tx
  end

  @spec a_shielded_transaction_with_intents() :: Transaction.t()
  defmemo a_shielded_transaction_with_intents do
    action = EAction.an_action_with_intents()
    priv_keys = <<3::256>>

    shielded_tx =
      %Transaction{
        actions: MapSet.new([action]),
        delta_proof: priv_keys
      }
      |> Transaction.prove_delta()

    assert true == Transaction.verify(shielded_tx)

    shielded_tx
  end

  @spec a_shielded_transaction_with_multiple_actions() :: Transaction.t()
  defmemo a_shielded_transaction_with_multiple_actions do
    an_action = EAction.an_action()
    another_action = EAction.an_action_with_intents()
    priv_keys = <<3::256>> <> <<3::256>>

    shielded_tx =
      %Transaction{
        actions: MapSet.new([an_action, another_action]),
        delta_proof: priv_keys
      }
      |> Transaction.prove_delta()

    assert true == Transaction.verify(shielded_tx)

    shielded_tx
  end

  @spec a_shielded_transaction_with_multiple_compliance_units() ::
          Transaction.t()
  defmemo a_shielded_transaction_with_multiple_compliance_units do
    an_action = EAction.an_action_with_multiple_compliance_units()
    priv_keys = <<6::256>>

    shielded_tx =
      %Transaction{
        actions: MapSet.new([an_action]),
        delta_proof: priv_keys
      }
      |> Transaction.prove_delta()

    assert true == Transaction.verify(shielded_tx)

    shielded_tx
  end

  @spec a_shielded_transaction_from_compliance_units() ::
          Transaction.t()
  defmemo a_shielded_transaction_from_compliance_units do
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

    shielded_tx = Transaction.prove_delta(pre_tx)

    assert true == Transaction.verify(shielded_tx)

    shielded_tx
  end

  @spec a_shielded_transaction_composition() :: Transaction.t()
  def a_shielded_transaction_composition do
    priv_keys = <<3::256>>

    shielded_tx_1 =
      %Transaction{
        actions: MapSet.new([EAction.an_action()]),
        delta_proof: priv_keys
      }

    shielded_tx_2 =
      %Transaction{
        actions: MapSet.new([EAction.an_action_with_intents()]),
        delta_proof: priv_keys
      }

    shielded_tx =
      Transaction.compose(shielded_tx_1, shielded_tx_2)
      |> Transaction.prove_delta()

    assert true == Transaction.verify(shielded_tx)

    shielded_tx
  end

  @spec a_invalid_shielded_transaction() :: Transaction.t()
  def a_invalid_shielded_transaction do
    action = EAction.an_action()
    # Use an invalid private key (all zeros) to create an invalid transaction
    invalid_priv_keys = <<0::256>>

    invalid_shielded_tx =
      %Transaction{
        actions: MapSet.new([action]),
        delta_proof: invalid_priv_keys
      }
      |> Transaction.prove_delta()

    assert {:error, "Delta proof verification failure"} ==
             Transaction.verify(invalid_shielded_tx)

    invalid_shielded_tx
  end

  @spec shielded_transaction_cipher_texts() :: [
          %{cipher: list(), tag: binary()}
        ]
  def shielded_transaction_cipher_texts(decryption_key \\ <<1::256>>) do
    cipher_texts =
      a_shielded_transaction_with_multiple_actions()
      |> Transaction.get_cipher_texts()

    for %{tag: _, cipher: c} <- cipher_texts do
      assert {:ok, _} = LogicInstance.decrypt(c, decryption_key)
    end

    cipher_texts
  end
end
