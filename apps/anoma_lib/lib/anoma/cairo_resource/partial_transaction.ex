defmodule Anoma.CairoResource.PartialTransaction do
  @moduledoc """
  I am a shielded resource machine partial transaction.
  """

  @behaviour Noun.Nounable.Kind

  require Logger

  alias __MODULE__
  use TypedStruct
  alias Anoma.CairoResource.{ProofRecord, ComplianceOutput, Tree, LogicOutput}
  alias Anoma.Constants

  typedstruct enforce: true do
    field(:logic_proofs, list(ProofRecord.t()), default: [])
    field(:compliance_proofs, list(ProofRecord.t()), default: [])
  end

  @spec from_noun(Noun.t()) :: {:ok, t()} | :error
  def from_noun([
        logic_proofs
        | compliance_proofs
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
      {:ok,
       %PartialTransaction{
         logic_proofs: Enum.map(logic, &elem(&1, 1)),
         compliance_proofs: Enum.map(compilance, &elem(&1, 1))
       }}
    else
      false -> :error
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

          compliance_hash_valid =
            ProofRecord.get_cairo_program_hash(proof_record) ==
              Constants.cairo_compliance_program_hash()

          Logger.debug("compliance result: #{inspect(result)}")
          acc && result && compliance_hash_valid
      end

    # Decode logic_outputs from resource logics
    logic_outputs =
      partial_transaction.logic_proofs
      |> Enum.map(fn proof_record ->
        proof_record.public_inputs
        |> LogicOutput.from_public_input()
      end)

    # Decode complaince_outputs from compliance_proofs
    complaince_outputs =
      partial_transaction.compliance_proofs
      |> Enum.map(fn proof_record ->
        proof_record.public_inputs
        |> ComplianceOutput.from_public_input()
      end)

    # Get self ids from logic_outputs
    self_ids = Enum.map(logic_outputs, & &1.self_resource_id)

    # Get cms and nfs from compliance_proofs
    resource_tree_leaves =
      complaince_outputs
      |> Enum.flat_map(fn output -> [output.nullifier, output.output_cm] end)

    # check self resource are all involved
    all_resource_valid = self_ids == resource_tree_leaves

    # Compute the expected resource tree
    rt =
      Tree.construct(
        CommitmentTree.Spec.cairo_poseidon_resource_tree_spec(),
        resource_tree_leaves
      )

    # check roots
    all_roots_valid =
      for output <- logic_outputs,
          reduce: true do
        acc ->
          acc && rt.root == output.root
      end

    all_logic_proofs_valid && all_compliance_proofs_valid &&
      all_resource_valid && all_roots_valid
  end

  defimpl Noun.Nounable, for: __MODULE__ do
    def to_noun(ptx = %PartialTransaction{}) do
      {ptx.logic_proofs, ptx.compliance_proofs}
      |> Noun.Nounable.to_noun()
    end
  end
end
