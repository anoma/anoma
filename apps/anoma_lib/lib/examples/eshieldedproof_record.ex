defmodule Examples.EShieldedProofRecord do
  alias Examples.EComplianceInput
  alias Anoma.RM.Shielded.ProofRecord

  @spec a_compliance_proof() :: %ProofRecord{}
  def a_compliance_proof do
    compliance_inputs = EComplianceInput.a_compliance_input()
    {:ok, proof} = ProofRecord.generate_compliance_proof(compliance_inputs)

    proof
  end
end
