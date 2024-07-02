defmodule Anoma.ShieldedResource.Transaction do
  @moduledoc """
  I am a shielded resource machine transaction.
  """

  use TypedStruct
  alias Anoma.ShieldedResource.ProofRecord

  typedstruct enforce: true do
    field(:logic_proofs, list(ProofRecord.t()), default: [])
    field(:compliance_proofs, list(ProofRecord.t()), default: [])
    field(:delta, binary(), default: %{})
    field(:extra, list(binary()), default: [])
    field(:preference, term(), default: nil)
  end
end
