defmodule Examples.ERisc0.EProofRecord do
  use Memoize

  alias Anoma.RM.Risc0.ProofRecord

  alias Examples.ERisc0.EComplianceWitness
  @spec a_compliance_proof() :: %ProofRecord{}

  defmemo a_compliance_proof do
    compliance_witness = EComplianceWitness.a_compliance_witness()
    {:ok, proof} = ProofRecord.generate_compliance_proof(compliance_witness)
    proof
  end
end
