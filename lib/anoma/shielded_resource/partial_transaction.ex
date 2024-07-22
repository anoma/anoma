defmodule Anoma.ShieldedResource.PartialTransaction do
  @moduledoc """
  I am a shielded resource machine partial transaction.
  """

  require Logger

  alias __MODULE__
  use TypedStruct
  alias Anoma.ShieldedResource.ProofRecord

  typedstruct enforce: true do
    field(:logic_proofs, list(ProofRecord.t()), default: [])
    field(:compliance_proofs, list(ProofRecord.t()), default: [])
  end

  @spec to_noun(t()) :: Noun.t()
  def to_noun(ptx = %PartialTransaction{}) do
    [
      for proof <- ptx.logic_proofs do
        ProofRecord.to_noun(proof)
      end,
      for proof <- ptx.compliance_proofs do
        ProofRecord.to_noun(proof)
      end
    ]
  end

  @spec from_noun(Noun.t()) :: t()
  def from_noun([
        logic_proofs,
        compliance_proofs
      ]) do
    %PartialTransaction{
      logic_proofs:
        for proof <- logic_proofs do
          ProofRecord.from_noun(proof)
        end,
      compliance_proofs:
        for proof <- compliance_proofs do
          ProofRecord.from_noun(proof)
        end
    }
  end

  @spec verify(t()) :: boolean()
  def verify(partial_transaction) do
    all_logic_proofs_valid =
      for proof_record <- partial_transaction.logic_proofs,
          reduce: true do
        acc ->
          result =
            Cairo.verify(proof_record.proof, proof_record.public_inputs)

          acc && result
      end

    all_compliance_proofs_valid =
      for proof_record <- partial_transaction.compliance_proofs,
          reduce: true do
        acc ->
          result =
            Cairo.verify(proof_record.proof, proof_record.public_inputs)

          Logger.debug("compliance result: #{inspect(result)}")
          acc && result
      end

    all_logic_proofs_valid && all_compliance_proofs_valid
  end
end
