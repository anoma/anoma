defmodule Examples.ERM.EShielded.EResourceLogic do
  use Memoize

  alias Anoma.RM.Shielded.ProofRecord
  alias Examples.ERM.EShielded.EProofRecord

  use TestHelper.TestMacro

  @spec a_input_resource_logic() :: ProofRecord.t()
  defmemo a_input_resource_logic() do
    EProofRecord.a_resource_logic(
      "params/trivial_input_resource_logic_witness.json"
    )
  end

  @spec a_output_resource_logic() :: ProofRecord.t()
  defmemo a_output_resource_logic() do
    EProofRecord.a_resource_logic(
      "params/trivial_output_resource_logic_witness.json"
    )
  end
end
