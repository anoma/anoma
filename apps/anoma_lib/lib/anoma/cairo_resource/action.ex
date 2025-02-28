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
    field(:created_commitments, list(<<_::256>>), default: [])
    field(:consumed_nullifiers, list(<<_::256>>), default: [])
    # logic_proofs Type: Map<Tag, (logic_hash, Proof)>
    field(:logic_proofs, %{binary() => {binary(), ProofRecord.t()}},
      default: {}
    )

    field(:compliance_units, MapSet.t(ProofRecord.t()), default: MapSet.new())
    # app_data Type: Map<Tag, list({field_element, DeletionCriterion})>. Right
    # now, the DeletionCriterion can be either 0 or 1 to indicate whether need
    # to store the field_element. It could be extended to more complex criterion
    # in the future.
    field(:app_data, %{<<_::256>> => list({<<_::256>>, <<_::256>>})},
      default: %{}
    )
  end

  @spec new(
          list(<<_::256>>),
          list(<<_::256>>),
          list(ProofRecord.t()),
          list(ProofRecord.t())
        ) :: t()
  def new(
        created_commitments,
        consumed_nullifiers,
        logic_proofs,
        compliance_units
      ) do
    logic_proof_map =
      Enum.into(logic_proofs, %{}, fn proof ->
        {proof.instance |> LogicInstance.get_tag(),
         {ProofRecord.get_cairo_program_hash(proof), proof}}
      end)

    app_data =
      Enum.map(logic_proofs, fn proof ->
        proof.instance |> LogicInstance.get_app_data_pair()
      end)
      |> Enum.filter(fn {_, app_data} -> app_data != [] end)
      |> Enum.into(%{})

    %Action{
      created_commitments: created_commitments,
      consumed_nullifiers: consumed_nullifiers,
      logic_proofs: logic_proof_map,
      compliance_units: compliance_units |> MapSet.new(),
      app_data: app_data
    }
  end

  @spec from_noun(Noun.t()) :: {:ok, t()} | :error
  def from_noun([
        created_commitments,
        consumed_nullifiers,
        logic_proofs
        | compliance_units
      ]) do
    to_noun_list = fn xs ->
      xs
      |> Noun.list_nock_to_erlang()
      |> Enum.map(&ProofRecord.from_noun/1)
    end

    logic = to_noun_list.(logic_proofs)
    compliance = to_noun_list.(compliance_units)
    checked = Enum.all?(logic ++ compliance, &(elem(&1, 0) == :ok))

    compliance_list = Enum.map(compliance, &elem(&1, 1))

    logic_map =
      Enum.into(logic, %{}, fn {:ok, proof} ->
        {proof.instance |> LogicInstance.get_tag(),
         {ProofRecord.get_cairo_program_hash(proof), proof}}
      end)

    app_data =
      Enum.map(logic, fn {:ok, proof} ->
        proof.instance |> LogicInstance.get_app_data_pair()
      end)
      |> Enum.filter(fn {_, app_data} -> app_data != [] end)
      |> Enum.into(%{})

    with true <- checked do
      {:ok,
       %Action{
         created_commitments:
           created_commitments |> Noun.list_nock_to_erlang(),
         consumed_nullifiers:
           consumed_nullifiers |> Noun.list_nock_to_erlang(),
         logic_proofs: logic_map,
         compliance_units: MapSet.new(compliance_list),
         app_data: app_data
       }}
    else
      false -> :error
    end
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

      # Check the consistence of cms and nfs in compliances
      nfs_cms_from_compliances =
        complaince_instances
        |> Enum.flat_map(fn instance ->
          [instance.nullifier, instance.output_cm]
        end)

      resource_tree_leaves =
        Enum.zip_with(
          action.consumed_nullifiers,
          action.created_commitments,
          &[&1, &2]
        )
        |> Enum.concat()

      is_cms_nfs_valid =
        MapSet.equal?(
          MapSet.new(nfs_cms_from_compliances),
          MapSet.new(resource_tree_leaves)
        )

      # Compute the expected resource tree root
      rt =
        Tree.construct(
          CommitmentTree.Spec.cairo_poseidon_resource_tree_spec(),
          resource_tree_leaves
        )

      # check correspondence between logic_proofs and compliance_units
      is_consistent =
        Enum.reduce_while(complaince_instances, true, fn complaince_instance,
                                                         _acc ->
          # check all the resource logic proofs are included
          res =
            with {:ok, {input_logic_hash, input_proof}} <-
                   Map.fetch(
                     action.logic_proofs,
                     complaince_instance.nullifier
                   ),
                 true <- ProofRecord.verify(input_proof),
                 {:ok, {output_logic_hash, output_proof}} <-
                   Map.fetch(
                     action.logic_proofs,
                     complaince_instance.output_cm
                   ),
                 true <- ProofRecord.verify(output_proof) do
              is_input_logic_valid =
                complaince_instance.input_logic_ref == input_logic_hash

              is_output_logic_valid =
                complaince_instance.output_logic_ref == output_logic_hash

              is_root_valid =
                rt.root ==
                  input_proof.instance |> LogicInstance.get_root() &&
                  rt.root ==
                    output_proof.instance |> LogicInstance.get_root()

              is_input_logic_valid && is_output_logic_valid && is_root_valid
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

      is_cms_nfs_valid && is_consistent
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

  defimpl Noun.Nounable, for: __MODULE__ do
    @impl true
    def to_noun(action = %Action{}) do
      {
        action.created_commitments,
        action.consumed_nullifiers,
        action.logic_proofs
        |> Enum.map(fn {_tag, {_hash, proof}} -> proof end),
        action.compliance_units |> MapSet.to_list()
      }
      |> Noun.Nounable.to_noun()
    end
  end
end
