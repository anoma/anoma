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
      Noun.Nounable.to_noun(ptx.logic_proofs),
      Noun.Nounable.to_noun(ptx.compliance_proofs)
    ]
  end

  @spec from_noun(Noun.t()) :: t()
  def from_noun([
        logic_proofs,
        compliance_proofs
      ]) do
    to_noun_list = fn xs ->
      xs
      |> Noun.list_nock_to_erlang()
      |> Enum.map(&ProofRecord.from_noun/1)
    end

    logic = to_noun_list.(logic_proofs)
    compilance = to_noun_list.(compliance_proofs)
    checked = Enum.all?(logic ++ compilance, &(elem(&1, 0) == :ok))

    with true <- checked do
      %PartialTransaction{
        logic_proofs: Enum.map(logic, &elem(&1, 1)),
        compliance_proofs: Enum.map(compilance, &elem(&1, 1))
      }
    end
  end

  @spec verify(t()) :: boolean()
  def verify(partial_transaction) do
    all_logic_proofs_valid =
      for proof_record <- partial_transaction.logic_proofs,
          reduce: true do
        acc ->
          result =
            proof_record.proof
            |> :binary.bin_to_list()
            |> Cairo.verify(
              proof_record.public_inputs
              |> :binary.bin_to_list()
            )

          acc && result
      end

    all_compliance_proofs_valid =
      for proof_record <- partial_transaction.compliance_proofs,
          reduce: true do
        acc ->
          result =
            proof_record.proof
            |> :binary.bin_to_list()
            |> Cairo.verify(
              proof_record.public_inputs
              |> :binary.bin_to_list()
            )

          Logger.debug("compliance result: #{inspect(result)}")
          acc && result
      end

    all_logic_proofs_valid && all_compliance_proofs_valid
  end
end
