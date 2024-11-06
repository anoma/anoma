defmodule Anoma.RM.Risc0.ComplianceWitness do
  @moduledoc """
  I represent a compliance witness.
  """

  use TypedStruct

  alias Anoma.RM.Shielded.Resource

  typedstruct enforce: true do
    # Input resource
    field(:input_resource, Resource.t())
    # Input resource merkle path
    field(:merkle_path, binary()) # TODO: change to CommitmentTree.Proof.t()
    # Nullifier key of the input resource
    field(:input_nf_key, binary(), default: <<0::256>>)
    # Ephemeral root
    field(:eph_root, binary(), default: <<0::256>>)
    # Output resource
    field(:output_resource, Resource.t())
    # Random value in delta proof(binding signature)
    field(:rcv, binary(), default: <<0::256>>)
  end

end
