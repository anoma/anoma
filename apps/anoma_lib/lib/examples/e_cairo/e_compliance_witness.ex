defmodule Examples.ECairo.EComplianceWitness do
  alias Anoma.CairoResource.Compliance.Witness, as: ComplianceWitness
  alias Anoma.CairoResource.Compliance.ComplianceUnit
  alias Examples.ECairo.EResource
  alias Examples.ECommitmentTree

  use TestHelper.TestMacro

  @spec a_compliance_witness :: binary()
  def a_compliance_witness() do
    input_nf_key = <<1::256>>
    rcv = <<3::256>>
    eph_root = Cairo.random_felt() |> :binary.list_to_bin()
    {_ct, merkle_proof, _anchor} = ECommitmentTree.a_merkle_proof()

    %ComplianceWitness{
      input_resource: EResource.a_fixed_resource(),
      merkel_proof: merkle_proof,
      output_resource: EResource.a_fixed_output_resource(),
      input_nf_key: input_nf_key,
      eph_root: eph_root,
      rcv: rcv
    }
    |> ComplianceWitness.to_json_string()
  end

  @spec an_invalid_compliance_witness :: binary()
  def an_invalid_compliance_witness() do
    empty_json = ""

    assert {:error, "Runtime error: The cairo program execution failed"} =
             ComplianceUnit.generate_compliance_proof(empty_json)

    invalid_compliance_witness = """
    {"eph_root": "0x4"}
    """

    assert {:error, "Runtime error: The cairo program execution failed"} =
             ComplianceUnit.generate_compliance_proof(
               invalid_compliance_witness
             )

    invalid_compliance_witness
  end

  @spec a_compliance_witness_for_intents :: binary()
  def a_compliance_witness_for_intents() do
    input_nf_key = <<1::256>>
    rcv = <<3::256>>
    eph_root = Anoma.Constants.default_cairo_rm_root()
    {_ct, merkle_proof, _anchor} = ECommitmentTree.a_merkle_proof()

    %ComplianceWitness{
      input_resource: EResource.a_trivial_input_intent_resource(),
      merkel_proof: merkle_proof,
      output_resource: EResource.a_trivial_output_intent_resource(),
      input_nf_key: input_nf_key,
      eph_root: eph_root,
      rcv: rcv
    }
    |> ComplianceWitness.to_json_string()
  end
end
