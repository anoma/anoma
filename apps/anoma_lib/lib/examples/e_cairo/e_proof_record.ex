defmodule Examples.ECairo.EProofRecord do
  alias Anoma.CairoResource.ProofRecord
  alias Anoma.CairoResource.Tree
  alias Examples.ECairo.EComplianceWitness

  use Memoize
  use TestHelper.TestMacro

  @spec a_compliance_proof() :: ProofRecord.t()
  defmemo a_compliance_proof do
    compliance_private_inputs =
      EComplianceWitness.a_compliance_private_input()

    assert {:ok, proof} =
             ProofRecord.generate_compliance_proof(compliance_private_inputs)

    proof
  end

  @spec a_resource_logic(binary(), list() | nil) :: ProofRecord.t()
  def a_resource_logic(input_file, path \\ nil) do
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

    updated_witness =
      if path do
        assert {:ok, updated_witness} = Tree.set_path(witness, path)
        updated_witness
      else
        witness
      end

    assert {:ok, input_resource_logic_proof} =
             ProofRecord.generate_cairo_proof(
               circuit,
               updated_witness
             )

    assert true = ProofRecord.verify(input_resource_logic_proof)

    input_resource_logic_proof
  end

  @spec a_compliance_proof_with_intents() :: ProofRecord.t()
  defmemo a_compliance_proof_with_intents do
    compliance_private_inputs =
      EComplianceWitness.a_compliance_private_input_for_intents()

    assert {:ok, proof} =
             ProofRecord.generate_compliance_proof(compliance_private_inputs)

    proof
  end
end
