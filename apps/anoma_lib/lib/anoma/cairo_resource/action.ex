defmodule Anoma.CairoResource.Action do
  @moduledoc """
  I am an action in shielded resource machine.
  """

  @behaviour Noun.Nounable.Kind

  alias __MODULE__
  alias Anoma.CairoResource.Compliance.ComplianceUnit
  alias Anoma.CairoResource.LogicInstance
  alias Anoma.CairoResource.ProofRecord
  alias Anoma.CairoResource.Tree
  alias Anoma.Constants
  alias Anoma.CairoResource.Resource
  alias Anoma.CairoResource.Utils
  alias Anoma.CairoResource.Workflow

  require Logger

  use TypedStruct

  typedstruct enforce: true do
    # resource_logic_proofs Type: Map<Tag, (logic_hash, Proof)>
    # (isConsumed, applicationData) is in the Proof.instance
    field(:resource_logic_proofs, %{binary() => {binary(), ProofRecord.t()}},
      default: {}
    )

    field(:compliance_units, list(ComplianceUnit.t()), default: [])
  end

  @spec create(
          list(Jason.OrderedObject.t()),
          list(binary()),
          list(Jason.OrderedObject.t()),
          list(binary()),
          list(Jason.OrderedObject.t())
        ) ::
          {:ok, {t(), binary()}} | {:error, term()}
  def create(
        compliance_units,
        input_logics,
        input_witnesses,
        output_logics,
        output_witnesses
      ) do
    with input_resource_jsons =
           Enum.map(compliance_units, & &1["input"]),
         output_resource_jsons =
           Enum.map(compliance_units, & &1["output"]),
         {:ok, input_nf_keys} <-
           Enum.map(
             compliance_units,
             &Utils.parse_json_field_to_binary32(&1, "input_nf_key")
           )
           |> Utils.check_list(),
         {:ok, input_resources} <-
           Workflow.get_input_resources(
             input_resource_jsons,
             input_logics,
             input_nf_keys
           ),
         input_nullifiers =
           Enum.zip_with(
             input_resources,
             input_nf_keys,
             &Resource.nullifier/2
           ),
         {:ok, output_resources} <-
           Workflow.get_output_resources(
             output_resource_jsons,
             output_logics,
             input_nullifiers
           ),
         output_commitments =
           Enum.map(output_resources, &Resource.commitment/1),
         {:ok, input_paths, output_paths} <-
           Workflow.create_merkle_tree_paths(
             input_nullifiers,
             output_commitments
           ),
         input_is_consumed_flags =
           Enum.map(input_resources, fn _ -> true end),
         {:ok, updated_input_witnesses} <-
           Workflow.update_witnesses(
             input_witnesses,
             input_resources,
             input_is_consumed_flags,
             input_nf_keys,
             input_paths
           ),
         output_is_consumed_flags =
           Enum.map(output_resources, fn _ -> false end),
         {:ok, updated_output_witnesses} <-
           Workflow.update_witnesses(
             output_witnesses,
             output_resources,
             output_is_consumed_flags,
             input_nf_keys,
             output_paths
           ),
         {:ok, input_logic_proofs} <-
           Workflow.generate_resource_logic_proofs(
             input_logics,
             updated_input_witnesses
           ),
         {:ok, output_logic_proofs} <-
           Workflow.generate_resource_logic_proofs(
             output_logics,
             updated_output_witnesses
           ),
         {:ok, compliance_witness} <-
           Workflow.create_compliance_inputs(
             compliance_units,
             input_resources,
             output_resources
           ),
         {:ok, compliance_proofs} <-
           Workflow.generate_compliance_proofs(compliance_witness),
         action =
           Workflow.create_action(
             input_logic_proofs,
             output_logic_proofs,
             compliance_proofs
           ),
         {:ok, delta_witness} <-
           Workflow.create_private_keys(compliance_units) do
      {:ok, {action, delta_witness}}
    else
      {:error, x} -> {:error, x}
    end
  end

  @spec new(
          list(ProofRecord.t()),
          list(ComplianceUnit.t())
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
    |> Enum.map(fn compliance_unit ->
      compliance_unit.instance.output_cm
    end)
  end

  @spec nullifiers(t()) :: list(binary())
  def nullifiers(action) do
    action.compliance_units
    |> Enum.map(fn compliance_unit ->
      compliance_unit.instance.nullifier
    end)
  end

  @spec roots(t()) :: list(binary())
  def roots(action) do
    action.compliance_units
    |> Enum.map(fn compliance_unit ->
      compliance_unit.instance.root
    end)
  end

  @spec delta(t()) :: list(binary())
  def delta(action) do
    action.compliance_units
    |> Enum.map(fn compliance_unit ->
      compliance_unit.instance.delta_x <> compliance_unit.instance.delta_y
    end)
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
           verify_compliance_proofs(action.compliance_units),
         true <- verify_compliance_hash(action.compliance_units) do
      # Decode compliance_instances from compliance_units
      complaince_instances =
        action.compliance_units
        |> Enum.map(fn compliance_unit ->
          compliance_unit.instance
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

  @spec verify_compliance_proofs(list(ComplianceUnit.t())) :: boolean()
  defp verify_compliance_proofs(proofs) do
    Enum.reduce_while(proofs, true, fn proof, _acc ->
      res = ComplianceUnit.verify(proof)

      case res do
        true -> {:cont, true}
        false -> {:halt, false}
        {:error, _} -> {:halt, false}
      end
    end)
  end

  @spec verify_compliance_hash(list(ComplianceUnit.t())) :: boolean()
  defp verify_compliance_hash(compliance_units) do
    compliance_units
    |> Enum.all?(
      &(&1.verifying_key == Constants.cairo_compliance_program_hash())
    )
  end

  @spec from_noun(Noun.t()) :: {:ok, Action.t()} | :error
  def from_noun([logic_proofs | compliance_units]) do
    with {:ok, logic_proofs_map} <- Noun.Nounable.Map.from_noun(logic_proofs),
         {:ok, compliance_unit_list} <-
           Noun.Nounable.List.from_noun(compliance_units) do
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
           compliance_unit_list
           |> Enum.map(fn unit ->
             {:ok, compliance_unit} = ComplianceUnit.from_noun(unit)
             compliance_unit
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
