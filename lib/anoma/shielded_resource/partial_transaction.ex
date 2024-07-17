defmodule Anoma.ShieldedResource.PartialTransaction do
  @moduledoc """
  I am a shielded resource machine partial transaction.
  """

  use TypedStruct
  alias Anoma.ShieldedResource.ProofRecord

  typedstruct enforce: true do
    field(:logic_proofs, list(ProofRecord.t()), default: [])
    field(:compliance_proofs, list(ProofRecord.t()), default: [])
  end
end
