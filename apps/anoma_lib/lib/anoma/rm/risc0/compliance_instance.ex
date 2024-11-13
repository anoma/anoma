defmodule Anoma.RM.Risc0.ComplianceInstance do
  @moduledoc """
  I represent the resource's public inputs.
  """

  use TypedStruct

  typedstruct enforce: true do
    # Input resource nullifier
    field(:input_nf, binary(), default: <<0::256>>)
    # Output resource commitment
    field(:output_cm, binary(), default: <<0::256>>)
    # Merkle tree root of input resouce
    field(:merkle_root, binary(), default: <<0::256>>)
    # Resource delta
    field(:delta, binary(), default: <<0::256>>)
    # Input Resource logic
    field(:input_logic, binary(), default: <<0::256>>)
    # Output Resource logic
    field(:output_logic, binary(), default: <<0::256>>)
  end

  @spec from_proof(binary()) ::
  Anoma.RM.Risc0.ComplianceInstance.t()
  def from_proof(proof) do
  ## call cairo api to get output bytes
  [input_nf, output_cm, input_logic, output_logic, merkle_root, delta] =
  proof |> :binary.bin_to_list() |> Risc0.get_compliance_instance()

    %Anoma.RM.Risc0.ComplianceInstance{
      input_nf: input_nf |> :binary.list_to_bin(),
      output_cm: output_cm |> :binary.list_to_bin(),
      merkle_root: merkle_root |> :binary.list_to_bin(),
      delta: delta |> :binary.list_to_bin(),
      input_logic: input_logic |> :binary.list_to_bin(),
      output_logic: output_logic |> :binary.list_to_bin()
    }
  end
end
