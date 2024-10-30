defmodule Examples.ERM.EShielded.EPartialTransaction do
  alias Anoma.RM.Shielded.PartialTransaction
  alias Examples.ERM.EShielded.EProofRecord

  @spec a_partial_transaction() :: %PartialTransaction{}
  def a_partial_transaction do
    proof = EProofRecord.a_compliance_proof()
    input_resource_logic = proof
    output_resource_logic = proof

    ptx = %PartialTransaction{
      logic_proofs: [input_resource_logic, output_resource_logic],
      compliance_proofs: [proof]
    }

    ptx
  end
end
