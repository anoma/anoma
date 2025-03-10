defmodule Examples.ECairo.EAction do
  alias Anoma.CairoResource.Action
  alias Anoma.CairoResource.Resource
  alias Anoma.CairoResource.Tree
  alias Examples.ECairo.EProofRecord
  alias Examples.ECairo.EResource
  alias Examples.ECairo.EResourceLogic

  use Memoize
  use TestHelper.TestMacro

  @spec an_action() :: Action.t()
  defmemo an_action do
    proof = EProofRecord.a_compliance_proof()
    input_resource_logic = EResourceLogic.a_input_resource_logic()
    output_resource_logic = EResourceLogic.a_output_resource_logic()

    action =
      Action.new([input_resource_logic, output_resource_logic], [proof])

    assert Action.verify(action)

    action
  end

  @spec an_action_with_intents() :: Action.t()
  defmemo an_action_with_intents do
    proof = EProofRecord.a_compliance_proof_with_intents()
    input_resource_logic = EResourceLogic.an_input_intent_resource_logic()
    output_resource_logic = EResourceLogic.an_output_intent_resource_logic()

    action =
      Action.new([input_resource_logic, output_resource_logic], [proof])

    assert Action.verify(action)

    action
  end

  @spec an_action_with_multiple_compliance_units() :: Action.t()
  defmemo an_action_with_multiple_compliance_units do
    # nf_key is used in the following resources
    nf_key = <<1::256>>

    # resources for compliance_unit_1
    input_resource_1 = EResource.a_fixed_resource()
    output_resource_1 = EResource.a_fixed_output_resource()
    compliance_unit_1 = EProofRecord.a_compliance_proof()

    # resources for compliance_unit_2
    input_resource_2 = EResource.a_trivial_input_intent_resource()
    output_resource_2 = EResource.a_trivial_output_intent_resource()
    compliance_unit_2 = EProofRecord.a_compliance_proof_with_intents()

    # construct the action tree
    input_resource_nf_1 = Resource.nullifier(input_resource_1, nf_key)
    output_resource_cm_1 = Resource.commitment(output_resource_1)
    input_resource_nf_2 = Resource.nullifier(input_resource_2, nf_key)
    output_resource_cm_2 = Resource.commitment(output_resource_2)

    rt =
      Tree.construct(
        CommitmentTree.Spec.cairo_poseidon_resource_tree_spec(),
        [
          input_resource_nf_1,
          output_resource_cm_1,
          input_resource_nf_2,
          output_resource_cm_2
        ]
      )

    # Generate logic proofs
    input_resource_path_1 = Tree.prove(rt, input_resource_nf_1)

    input_resource_logic_proof_1 =
      EProofRecord.a_resource_logic(
        "params/trivial_input_resource_logic_witness.json",
        input_resource_path_1
      )

    output_resource_path_1 = Tree.prove(rt, output_resource_cm_1)

    output_resource_logic_proof_1 =
      EProofRecord.a_resource_logic(
        "params/trivial_output_resource_logic_witness.json",
        output_resource_path_1
      )

    input_resource_path_2 = Tree.prove(rt, input_resource_nf_2)

    input_resource_logic_proof_2 =
      EProofRecord.a_resource_logic(
        "params/trivial_input_intent_resource_logic_witness.json",
        input_resource_path_2
      )

    output_resource_path_2 = Tree.prove(rt, output_resource_cm_2)

    output_resource_logic_proof_2 =
      EProofRecord.a_resource_logic(
        "params/trivial_output_intent_resource_logic_witness.json",
        output_resource_path_2
      )

    action =
      Action.new(
        [
          input_resource_logic_proof_1,
          input_resource_logic_proof_2,
          output_resource_logic_proof_1,
          output_resource_logic_proof_2
        ],
        [compliance_unit_1, compliance_unit_2]
      )

    assert Action.verify(action)

    action
  end
end
