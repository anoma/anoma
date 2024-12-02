defmodule Examples.ECairo.EAction do
  alias Anoma.CairoResource.Action
  alias Examples.ECairo.EProofRecord
  alias Examples.ECairo.EResourceLogic

  use TestHelper.TestMacro

  @spec an_action() :: Action.t()
  def an_action do
    proof = EProofRecord.a_compliance_proof()
    input_resource_logic = EResourceLogic.a_input_resource_logic()
    output_resource_logic = EResourceLogic.a_output_resource_logic()

    action = %Action{
      logic_proofs: [input_resource_logic, output_resource_logic],
      compliance_proofs: [proof]
    }

    assert Action.verify(action)

    action
  end

  @spec an_action_with_intents() :: Action.t()
  def an_action_with_intents do
    proof = EProofRecord.a_compliance_proof_with_intents()
    input_resource_logic = EResourceLogic.an_input_intent_resource_logic()
    output_resource_logic = EResourceLogic.an_output_intent_resource_logic()

    action = %Action{
      logic_proofs: [input_resource_logic, output_resource_logic],
      compliance_proofs: [proof]
    }

    assert Action.verify(action)

    action
  end
end
