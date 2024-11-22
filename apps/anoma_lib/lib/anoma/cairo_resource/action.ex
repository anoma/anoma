defmodule Anoma.CairoResource.Action do
  @moduledoc """
  I am an action in shielded resource machine.
  """

  @behaviour Noun.Nounable.Kind

  require Logger

  alias __MODULE__
  use TypedStruct

  alias Anoma.CairoResource.{
    ProofRecord,
    ComplianceInstance,
    Tree,
    LogicInstance
  }

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
       %Action{
         logic_proofs: Enum.map(logic, &elem(&1, 1)),
         compliance_proofs: Enum.map(compilance, &elem(&1, 1))
       }}
    else
      false -> :error
    end
  end

  @spec verify(t()) :: boolean()
  def verify(action) do
    with true <- verify_proofs(action.logic_proofs),
         true <-
           verify_proofs(
             action.compliance_proofs,
             "compliance result"
           ),
         true <- verify_compliance_hash(action.compliance_proofs) do
      # Decode logic_instances from resource logics
      logic_instances =
        action.logic_proofs
        |> Enum.map(fn proof_record ->
          proof_record.public_inputs
          |> LogicInstance.from_public_input()
        end)

      # Decode complaince_instance from compliance_proofs
      complaince_instance =
        action.compliance_proofs
        |> Enum.map(fn proof_record ->
          proof_record.public_inputs
          |> ComplianceInstance.from_public_input()
        end)

      # Get self ids from logic_instances
      self_ids = Enum.map(logic_instances, & &1.tag)

      # Get cms and nfs from compliance_proofs
      resource_tree_leaves =
        complaince_instance
        |> Enum.flat_map(fn instance ->
          [instance.nullifier, instance.output_cm]
        end)

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
        for instance <- logic_instances,
            reduce: true do
          acc ->
            acc && rt.root == instance.root
        end

      all_resource_valid && all_roots_valid
    else
      _ -> false
    end
  end

  @spec verify_proofs(list(ProofRecord.t()), binary() | nil) :: boolean()
  defp verify_proofs(proofs, debug_msg \\ nil) do
    Enum.reduce_while(proofs, true, fn proof_record, _acc ->
      res = ProofRecord.verify(proof_record)

      if debug_msg do
        Logger.debug("#{debug_msg}: #{inspect(res)}")
      end

      case res do
        true -> {:cont, true}
        false -> {:halt, false}
        {:error, _} -> {:halt, false}
      end
    end)
  end

  @spec verify_compliance_hash(list(ProofRecord.t())) :: boolean()
  def verify_compliance_hash(compliance_proofs) do
    compliance_proofs
    |> Enum.all?(
      &(ProofRecord.get_cairo_program_hash(&1) ==
          Constants.cairo_compliance_program_hash())
    )
  end

  defimpl Noun.Nounable, for: __MODULE__ do
    @impl true
    def to_noun(action = %Action{}) do
      {action.logic_proofs, action.compliance_proofs}
      |> Noun.Nounable.to_noun()
    end
  end
end
