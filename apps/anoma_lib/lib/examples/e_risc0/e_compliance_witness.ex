defmodule Examples.ERisc0.EComplianceWitness do
  alias Anoma.RM.Risc0.ComplianceWitness

  alias Examples.ERisc0.EResource

  @spec a_compliance_witness :: %ComplianceWitness{}
  def a_compliance_witness() do
    nsk = Risc0.random_32()
    rcv = Risc0.random_32()
    merkle_path = Risc0.random_merkle_path_32()

    compliance_witness =
      %ComplianceWitness{
        input_resource: EResource.a_resource(nsk: nsk),
        output_resource: EResource.a_resource(nsk: nsk),
        rcv: rcv |> :binary.list_to_bin(),
        merkle_path: merkle_path,
        nsk: nsk |> :binary.list_to_bin(),
      }

    IO.puts("Compliance witness: #{inspect(compliance_witness)}")

    compliance_witness
  end
end
