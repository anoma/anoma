defmodule Examples.EComplianceInput do
  alias Anoma.ShieldedResource.ComplianceInput
  alias Examples.{EShieldedResource, ECommitmentTree}

  @spec a_compliance_input :: binary()
  def a_compliance_input() do

    input_nf_key = :binary.copy(<<0>>, 31) <> <<1>>
    rcv = :binary.copy(<<0>>, 31) <> <<3>>
    eph_root = Cairo.random_felt() |> :binary.list_to_bin()
    {_ct, merkle_proof, _anchor} = ECommitmentTree.a_merkle_proof()

    compliance_input =
      %ComplianceInput{
        input_resource: EShieldedResource.a_resource(),
        merkel_proof: merkle_proof,
        output_resource: EShieldedResource.a_output_resource(),
        input_nf_key: input_nf_key,
        eph_root: eph_root,
        rcv: rcv
      }
      |> ComplianceInput.to_json_string()

    compliance_input
  end

end