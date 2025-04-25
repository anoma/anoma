defmodule Anoma.CairoResource.Action do
  @moduledoc """
  I am an action in shielded resource machine.
  """

  @behaviour Noun.Nounable.Kind

  alias __MODULE__
  alias Anoma.CairoResource.ComplianceInstance
  alias Anoma.CairoResource.LogicInstance
  alias Anoma.CairoResource.ProofRecord
  alias Anoma.CairoResource.Tree
  alias Anoma.Constants

  require Logger

  use TypedStruct

  typedstruct enforce: true do
    # resource_logic_proofs Type: Map<Tag, (logic_hash, Proof)>
    # (isConsumed, applicationData) is in the Proof.instance
    field(:resource_logic_proofs, %{binary() => {binary(), ProofRecord.t()}},
      default: {}
    )

    field(:compliance_units, list(ProofRecord.t()), default: [])
  end

  @spec new(
          list(ProofRecord.t()),
          list(ProofRecord.t())
        ) :: t()
  def new(
        resource_logic_proofs,
        compliance_units
      ) do
    logic_proof_map =
      Enum.into(resource_logic_proofs, %{}, fn proof ->
        {proof.instance |> LogicInstance.get_tag(),
         {ProofRecord.get_cairo_program_hash(proof), proof}}
      end)

    %Action{
      resource_logic_proofs: logic_proof_map,
      compliance_units: compliance_units
    }
  end

  @spec commitments(t()) :: list(binary())
  def commitments(action) do
    action.compliance_units
    |> Enum.map(fn proof_record ->
      proof_record.instance
      |> ComplianceInstance.from_public_input()
    end)
    |> Enum.map(& &1.output_cm)
  end

  @spec nullifiers(t()) :: list(binary())
  def nullifiers(action) do
    action.compliance_units
    |> Enum.map(fn proof_record ->
      proof_record.instance
      |> ComplianceInstance.from_public_input()
    end)
    |> Enum.map(& &1.nullifier)
  end

  @spec app_data(t()) :: list({<<_::256>>, <<_::256>>})
  def app_data(action) do
    action.resource_logic_proofs
    |> Enum.flat_map(fn {_tag, {_logic_hash, proof_record}} ->
      proof_record.instance
      |> LogicInstance.get_app_data()
    end)
  end

  @spec verify(t()) :: boolean()
  def verify(action) do
    with true <-
           verify_proofs(action.compliance_units),
         true <- verify_compliance_hash(action.compliance_units) do
      # Decode compliance_instances from compliance_units
      complaince_instances =
        action.compliance_units
        |> Enum.map(fn proof_record ->
          proof_record.instance
          |> ComplianceInstance.from_public_input()
        end)

      # Get all the nullifiers and commitments
      resource_tree_leaves =
        complaince_instances
        |> Enum.flat_map(fn instance ->
          [instance.nullifier, instance.output_cm]
        end)

      # Generate the expected action tree root
      rt =
        Tree.construct(
          CommitmentTree.Spec.cairo_poseidon_resource_tree_spec(),
          resource_tree_leaves
        )

      # check correspondence between resource_logic_proofs and compliance_units
      Enum.reduce_while(complaince_instances, true, fn complaince_instance,
                                                       _acc ->
        # check all the resource logic proofs are included and valid
        res =
          with {:ok, {consumed_logic_hash, consumed_logic_proof}} <-
                 Map.fetch(
                   action.resource_logic_proofs,
                   complaince_instance.nullifier
                 ),
               true <- ProofRecord.verify(consumed_logic_proof),
               {:ok, {created_logic_hash, created_logic_proof}} <-
                 Map.fetch(
                   action.resource_logic_proofs,
                   complaince_instance.output_cm
                 ),
               true <- ProofRecord.verify(created_logic_proof) do
            is_consumed_logic_consistent =
              complaince_instance.input_logic_ref == consumed_logic_hash

            is_created_logic_consistent =
              complaince_instance.output_logic_ref == created_logic_hash

            is_root_valid =
              rt.root ==
                consumed_logic_proof.instance |> LogicInstance.get_root() &&
                rt.root ==
                  created_logic_proof.instance |> LogicInstance.get_root()

            is_consumed_logic_consistent && is_created_logic_consistent &&
              is_root_valid
          else
            _ -> false
          end

        case res do
          true ->
            {:cont, true}

          false ->
            {:halt, false}
        end
      end)
    else
      _ -> false
    end
  end

  @spec verify_proofs(list(ProofRecord.t())) :: boolean()
  defp verify_proofs(proofs) do
    Enum.reduce_while(proofs, true, fn proof_record, _acc ->
      res = ProofRecord.verify(proof_record)

      case res do
        true -> {:cont, true}
        false -> {:halt, false}
        {:error, _} -> {:halt, false}
      end
    end)
  end

  @spec verify_compliance_hash(list(ProofRecord.t())) :: boolean()
  defp verify_compliance_hash(compliance_units) do
    compliance_units
    |> Enum.all?(
      &(ProofRecord.get_cairo_program_hash(&1) ==
          Constants.cairo_compliance_program_hash())
    )
  end

  @spec from_noun(Noun.t()) :: {:ok, Action.t()} | :error
  def from_noun([logic_proofs | compliance_proofs]) do
    with {:ok, logic_proofs_map} <- Noun.Nounable.Map.from_noun(logic_proofs),
         {:ok, compliance_proof_list} <-
           Noun.Nounable.List.from_noun(compliance_proofs) do
      {:ok,
       %__MODULE__{
         resource_logic_proofs:
           logic_proofs_map
           |> Enum.into(%{}, fn {tag, [bin | proof]} ->
             {:ok, pr} = ProofRecord.from_noun(proof)

             {Noun.atom_integer_to_binary(tag, 32),
              {Noun.atom_integer_to_binary(bin), pr}}
           end),
         compliance_units:
           compliance_proof_list
           |> Enum.map(fn proof ->
             {:ok, pr} = ProofRecord.from_noun(proof)
             pr
           end)
       }}
    else
      _ -> :error
    end
  end

  defimpl Noun.Nounable, for: __MODULE__ do
    @impl true
    def to_noun(action = %Action{}) do
      {
        action.resource_logic_proofs,
        action.compliance_units
      }
      |> Noun.Nounable.to_noun()
    end
  end
end
