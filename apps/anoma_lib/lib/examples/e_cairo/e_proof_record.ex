defmodule Examples.ECairo.EProofRecord do
  use Memoize

  alias Examples.ECairo.EComplianceWitness
  alias Anoma.CairoResource.ProofRecord

  use TestHelper.TestMacro

  @spec a_compliance_proof() :: ProofRecord.t()
  defmemo a_compliance_proof do
    compliance_private_inputs =
      EComplianceWitness.a_compliance_private_input()

    assert {:ok, proof} =
             ProofRecord.generate_compliance_proof(compliance_private_inputs)

    proof
  end

  @spec a_resource_logic(binary()) :: ProofRecord.t()
  def a_resource_logic(input_file) do
    witness_dir =
      Path.join(
        :code.priv_dir(:anoma_lib),
        input_file
      )

    circuit_dir =
      Path.join(
        :code.priv_dir(:anoma_lib),
        "params/trivial_resource_logic.json"
      )

    assert {:ok, circuit} = File.read(circuit_dir)
    assert {:ok, witness} = File.read(witness_dir)

    assert {:ok, input_resource_logic_proof} =
             ProofRecord.generate_cairo_proof(
               circuit,
               witness
             )

    assert true = ProofRecord.verify(input_resource_logic_proof)

    input_resource_logic_proof
  end
end
