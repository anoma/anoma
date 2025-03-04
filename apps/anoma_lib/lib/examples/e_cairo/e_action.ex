defmodule Examples.ECairo.EAction do
  alias Anoma.CairoResource.Action
  alias Examples.ECairo.EProofRecord
  alias Examples.ECairo.EResourceLogic
  alias Examples.ECairo.EResource
  alias Anoma.CairoResource.Tree

  use Memoize
  use TestHelper.TestMacro

  @spec an_action() :: Action.t()
  defmemo an_action do
    proof = EProofRecord.a_compliance_proof()
    input_resource_logic = EResourceLogic.a_input_resource_logic()
    output_resource_logic = EResourceLogic.a_output_resource_logic()

    action =
      Action.new(
        [EResource.a_fixed_output_resource_commitment()],
        [EResource.a_resource_nullifier()],
        [input_resource_logic, output_resource_logic],
        [proof]
      )

    assert Action.verify(action)

    action
  end

  @spec an_action_with_intents() :: Action.t()
  defmemo an_action_with_intents do
    proof = EProofRecord.a_compliance_proof_with_intents()
    input_resource_logic = EResourceLogic.an_input_intent_resource_logic()
    output_resource_logic = EResourceLogic.an_output_intent_resource_logic()

    action =
      Action.new(
        [EResource.a_trivial_output_intent_resource_commitment()],
        [EResource.a_trivial_input_intent_resource_nullifier()],
        [input_resource_logic, output_resource_logic],
        [proof]
      )

    assert Action.verify(action)

    action
  end

  @spec an_action_with_multiple_compliance_units() :: Action.t()
  defmemo an_action_with_multiple_compliance_units do
    compliance_unit_1 = EProofRecord.a_compliance_proof()
    compliance_unit_2 = EProofRecord.a_compliance_proof_with_intents()

    # construct the action tree
    input_resource_nf_1 = EResource.a_resource_nullifier()
    output_resource_cm_1 = EResource.a_fixed_output_resource_commitment()

    input_resource_nf_2 =
      EResource.a_trivial_input_intent_resource_nullifier()

    output_resource_cm_2 =
      EResource.a_trivial_output_intent_resource_commitment()

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
        [output_resource_cm_1, output_resource_cm_2],
        [input_resource_nf_1, input_resource_nf_2],
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
