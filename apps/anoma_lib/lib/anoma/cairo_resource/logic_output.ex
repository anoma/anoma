defmodule Anoma.CairoResource.LogicOutput do
  @moduledoc """
  I represent a resource logic's output.
  """

  use TypedStruct

  typedstruct enforce: true do
    # Self resource identity: nullifier of input resource or commitment of output resource
    field(:self_resource_id, binary())
    # The merkle root of resources in ptx
    field(:root, binary())
    # Ciphertext
    field(:cipher, list(binary()))
    # Custom outputs
    field(:others, list(binary()))
  end

  @spec from_public_input(binary()) :: t()
  def from_public_input(public_input) do
    ## call cairo api to get output bytes
    [
      self_resource_id,
      root,
      cipher_text_elem0,
      cipher_text_elem1,
      cipher_text_elem2,
      cipher_text_elem3,
      cipher_text_elem4,
      cipher_text_elem5,
      cipher_text_elem6,
      cipher_text_elem7,
      cipher_text_elem8,
      cipher_text_elem9,
      mac,
      pk_x,
      pk_y,
      nonce | others
    ] =
      public_input |> :binary.bin_to_list() |> Cairo.get_output()

    %__MODULE__{
      self_resource_id: self_resource_id |> :binary.list_to_bin(),
      root: root |> :binary.list_to_bin(),
      cipher: [
        cipher_text_elem0 |> :binary.list_to_bin(),
        cipher_text_elem1 |> :binary.list_to_bin(),
        cipher_text_elem2 |> :binary.list_to_bin(),
        cipher_text_elem3 |> :binary.list_to_bin(),
        cipher_text_elem4 |> :binary.list_to_bin(),
        cipher_text_elem5 |> :binary.list_to_bin(),
        cipher_text_elem6 |> :binary.list_to_bin(),
        cipher_text_elem7 |> :binary.list_to_bin(),
        cipher_text_elem8 |> :binary.list_to_bin(),
        cipher_text_elem9 |> :binary.list_to_bin(),
        mac |> :binary.list_to_bin(),
        pk_x |> :binary.list_to_bin(),
        pk_y |> :binary.list_to_bin(),
        nonce |> :binary.list_to_bin()
      ],
      others: others |> Enum.map(&:binary.list_to_bin/1)
    }
  end

  @spec decrypt(list(binary()), binary()) :: list(binary())
  def decrypt(cihper, sk) do
    cihper
    |> Enum.map(&:binary.bin_to_list/1)
    |> Cairo.decrypt(:binary.bin_to_list(sk))
    |> Enum.map(&:binary.list_to_bin/1)
  end
end
