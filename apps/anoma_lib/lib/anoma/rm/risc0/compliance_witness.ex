defmodule Anoma.RM.Risc0.ComplianceWitness do
  @moduledoc """
  I represent a compliance witness.
  """

  use TypedStruct

  alias Anoma.RM.Shielded.Resource

  typedstruct enforce: true do
    # Input resource
    field(:input_resource, Resource.t())
    # Output resource
    field(:output_resource, Resource.t())
    # Input resource merkle path
    field(:merkle_path, list(byte())) # TODO: change to CommitmentTree.Proof.t()
    # Nullifier key of the input resource
    field(:nsk, list(byte()))
    # Random value in delta proof(binding signature)
    field(:rcv, list(byte()))
  end

end
