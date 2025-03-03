defmodule Anoma.CairoResource.ComplianceWitness do
  @moduledoc """
  I represent a compliance's private inputs.
  """

  alias __MODULE__
  alias Anoma.CairoResource.Resource

  use TypedStruct

  typedstruct enforce: true do
    # Input resource
    field(:input_resource, Resource.t())
    # Input resource merkle path
    field(:merkel_proof, CommitmentTree.Proof.t())
    # Nullifier key of the input resource
    field(:input_nf_key, <<_::256>>, default: <<0::256>>)
    # Ephemeral root
    field(:eph_root, <<_::256>>, default: <<0::256>>)
    # Output resource
    field(:output_resource, Resource.t())
    # Random value in delta proof(binding signature)
    field(:rcv, <<_::256>>, default: <<0::256>>)
  end

  @doc "Generate the compliance witness json"
  @spec to_json_string(t()) :: binary()
  def to_json_string(input = %ComplianceWitness{}) do
    {_, _, path} =
      Enum.reduce(
        1..32,
        {input.merkel_proof.path, input.merkel_proof.proof, []},
        fn _, {path, proof, acc} ->
          {Integer.floor_div(path, 2), elem(proof, Integer.mod(path, 2)),
           [
             elem(proof, Integer.mod(path + 1, 2)) |> :binary.bin_to_list()
             | acc
           ]}
        end
      )

    Cairo.generate_compliance_input_json(
      Resource.to_bytes(input.input_resource),
      Resource.to_bytes(input.output_resource),
      path,
      input.merkel_proof.path,
      input.input_nf_key |> :binary.bin_to_list(),
      input.eph_root |> :binary.bin_to_list(),
      input.rcv |> :binary.bin_to_list()
    )
  end
end
