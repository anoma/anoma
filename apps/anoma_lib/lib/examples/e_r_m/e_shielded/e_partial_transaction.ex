defmodule Examples.ERM.EShielded.EPartialTransaction do
  alias Anoma.RM.Shielded.PartialTransaction
  alias Examples.ERM.EShielded.EProofRecord
  alias Examples.ERM.EShielded.EResourceLogic

  use TestHelper.TestMacro

  @spec a_partial_transaction() :: %PartialTransaction{}
  def a_partial_transaction do
    proof = EProofRecord.a_compliance_proof()
    input_resource_logic = EResourceLogic.a_input_resource_logic()
    output_resource_logic = EResourceLogic.a_output_resource_logic()

    ptx = %PartialTransaction{
      logic_proofs: [input_resource_logic, output_resource_logic],
      compliance_proofs: [proof]
    }

    assert PartialTransaction.verify(ptx)

    ptx
  end
end
