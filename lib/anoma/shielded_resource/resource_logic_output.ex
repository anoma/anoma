defmodule Anoma.ShieldedResource.ResourceLogicOutput do
  @moduledoc """
  I represent a resource logic's output.
  """

  use TypedStruct

  typedstruct enforce: true do
    # Self resource identity: nullifier of input resource or commitment of output resource
    field(:self_resource_id, binary())
    # The merkle root of resources in ptx
    field(:root, binary())
    # Other outputs to be defined
    field(:others, list(binary()))
  end

  @spec from_public_input(binary()) ::
          Anoma.ShieldedResource.ResourceLogicOutput.t()
  def from_public_input(public_input) do
    ## call cairo api to get output bytes
    [self_resource_id, root | others] =
      public_input |> :binary.bin_to_list() |> Cairo.get_output()

    %Anoma.ShieldedResource.ResourceLogicOutput{
      self_resource_id: self_resource_id |> :binary.list_to_bin(),
      root: root |> :binary.list_to_bin(),
      others: others |> Enum.map(&:binary.list_to_bin/1)
    }
  end
end
