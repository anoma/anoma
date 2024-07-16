defmodule Anoma.SheildedResource.ComplianceOutput do
  @moduledoc """
  I represent a resource's output.
  """
  use TypedStruct

  typedstruct enforce: true do
    # Nullifier
    field(:nullifier, binary())
    # Output output commitment
    field(:output_cm, binary())
    # Resource commitment Merkle tree root
    field(:root, binary())
    # Resource delta
    field(:delta_x, binary())
    field(:delta_y, binary())
    # Input Resource label
    field(:input_label, binary())
    # Output Resource label
    field(:output_label, binary())
  end

  def from_public_input(public_input) do
    ## call cairo api to get output bytes
    [nullifier, output_cm, root, delta_x, delta_y, input_label, output_label] =
      Cairo.get_compliance_output(public_input)

    %Anoma.SheildedResource.ComplianceOutput{
      nullifier: nullifier,
      output_cm: output_cm,
      root: root,
      delta_x: delta_x,
      delta_y: delta_y,
      input_label: input_label,
      output_label: output_label
    }
  end
end
