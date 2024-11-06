defmodule Examples.ERisc0.EComplianceWitness do
  alias Anoma.RM.Risc0.ComplianceWitness

  alias Examples.ERisc0.EResource

  @spec a_compliance_witness :: %ComplianceWitness{}
  def a_compliance_witness() do
    input_nf_key = <<1::256>>
    rcv = <<3::256>>
    eph_root = Risc0.random_32() |> :binary.list_to_bin()
    merkle_path = Risc0.random_merkle_path_32() |> :binary.list_to_bin()

    compliance_witness =
      %ComplianceWitness{
        input_resource: EResource.a_resource(),
        merkle_path: merkle_path,
        output_resource: EResource.a_output_resource(),
        input_nf_key: input_nf_key,
        eph_root: eph_root,
        rcv: rcv
      }

    compliance_witness
  end
end
