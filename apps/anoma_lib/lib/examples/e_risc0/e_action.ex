defmodule Examples.ERisc0.EAction do
  alias Anoma.RM.Risc0.Action
  alias Examples.ERisc0.EProofRecord
  alias Examples.ERisc0.EResourceLogic

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
end
