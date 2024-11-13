defmodule Examples.ERisc0.EProofRecord do
  use Memoize
  import ExUnit.Assertions

  alias Anoma.RM.Risc0.ProofRecord

  alias Examples.ERisc0.EComplianceWitness
  @spec a_compliance_proof() :: %ProofRecord{}

  defmemo a_compliance_proof do
    compliance_witness = EComplianceWitness.a_compliance_witness()
    {:ok, proof} = ProofRecord.generate_compliance_proof(compliance_witness)
    proof
  end

  @spec a_resource_logic(binary(), binary(), binary()) :: ProofRecord.t()
  def a_resource_logic(witness, elf_dir, guest_id) do
    elf = File.read!(elf_dir)

    assert {:ok, input_resource_logic_proof} =
             ProofRecord.generate_risc0_proof(witness, elf)

    assert true = ProofRecord.verify(input_resource_logic_proof, guest_id)

    input_resource_logic_proof
  end
end
