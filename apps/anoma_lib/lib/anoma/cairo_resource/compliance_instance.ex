defmodule Anoma.CairoResource.ComplianceInstance do
  @moduledoc """
  I represent the resource's instance.
  """

  use TypedStruct

  typedstruct enforce: true do
    # Input resource nullifier
    field(:nullifier, <<_::256>>, default: <<0::256>>)
    # Output resource commitment
    field(:output_cm, <<_::256>>, default: <<0::256>>)
    # Merkle tree root of input resouce
    field(:root, <<_::256>>, default: <<0::256>>)
    # Resource delta
    field(:delta_x, <<_::256>>, default: <<0::256>>)
    field(:delta_y, <<_::256>>, default: <<0::256>>)
    # Input Resource logic reference
    field(:input_logic_ref, <<_::256>>, default: <<0::256>>)
    # Output Resource logic reference
    field(:output_logic_ref, <<_::256>>, default: <<0::256>>)
  end

  @spec from_public_input(binary()) :: t()
  def from_public_input(public_input) do
    ## call cairo api to get output bytes
    [
      nullifier,
      output_cm,
      root,
      delta_x,
      delta_y,
      input_logic_ref,
      output_logic_ref
    ] =
      public_input |> :binary.bin_to_list() |> Cairo.get_output()

    %__MODULE__{
      nullifier: nullifier |> :binary.list_to_bin(),
      output_cm: output_cm |> :binary.list_to_bin(),
      root: root |> :binary.list_to_bin(),
      delta_x: delta_x |> :binary.list_to_bin(),
      delta_y: delta_y |> :binary.list_to_bin(),
      input_logic_ref: input_logic_ref |> :binary.list_to_bin(),
      output_logic_ref: output_logic_ref |> :binary.list_to_bin()
    }
  end
end
