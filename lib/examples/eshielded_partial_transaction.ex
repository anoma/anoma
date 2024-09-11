defmodule Examples.EShieldedPartialTransaction do
  alias Anoma.ShieldedResource.PartialTransaction
  alias Examples.EShieldedProofRecord


  @spec a_partial_transaction() :: %PartialTransaction{}
  def a_partial_transaction do
    proof = EShieldedProofRecord.a_compliance_proof()
    input_resource_logic = proof
    output_resource_logic = proof

    ptx = %PartialTransaction{
      logic_proofs: [input_resource_logic, output_resource_logic],
      compliance_proofs: [proof]
    }

    ptx
  end

end
