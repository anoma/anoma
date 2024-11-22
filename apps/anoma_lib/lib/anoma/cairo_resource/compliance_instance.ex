defmodule Anoma.CairoResource.ComplianceInstance do
  @moduledoc """
  I represent the resource's public inputs.
  """

  use TypedStruct

  typedstruct enforce: true do
    # Input resource nullifier
    field(:nullifier, binary(), default: <<0::256>>)
    # Output resource commitment
    field(:output_cm, binary(), default: <<0::256>>)
    # Merkle tree root of input resouce
    field(:root, binary(), default: <<0::256>>)
    # Resource delta
    field(:delta_x, binary(), default: <<0::256>>)
    field(:delta_y, binary(), default: <<0::256>>)
    # Input Resource logic
    field(:input_logic, binary(), default: <<0::256>>)
    # Output Resource logic
    field(:output_logic, binary(), default: <<0::256>>)
  end

  @spec from_public_input(binary()) :: t()
  def from_public_input(public_input) do
    ## call cairo api to get output bytes
    [nullifier, output_cm, root, delta_x, delta_y, input_logic, output_logic] =
      public_input |> :binary.bin_to_list() |> Cairo.get_output()

    %__MODULE__{
      nullifier: nullifier |> :binary.list_to_bin(),
      output_cm: output_cm |> :binary.list_to_bin(),
      root: root |> :binary.list_to_bin(),
      delta_x: delta_x |> :binary.list_to_bin(),
      delta_y: delta_y |> :binary.list_to_bin(),
      input_logic: input_logic |> :binary.list_to_bin(),
      output_logic: output_logic |> :binary.list_to_bin()
    }
  end
end
