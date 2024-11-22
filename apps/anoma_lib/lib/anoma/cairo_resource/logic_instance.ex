defmodule Anoma.CairoResource.LogicInstance do
  @moduledoc """
  I represent the resource logic's public inputs.
  """

  use TypedStruct

  typedstruct enforce: true do
    # nullifier of input resource or commitment of output resource
    field(:tag, binary(), default: <<0::256>>)
    # The merkle root of resources in current action(execution context)
    field(:root, binary(), default: <<0::256>>)
    # Ciphertext
    field(:cipher, list(binary()), default: [])
    # Custom public inputs
    field(:app_data, list(binary()), default: [])
  end

  @spec from_public_input(binary()) :: t()
  def from_public_input(public_input) do
    # call cairo api to get output bytes
    [
      tag,
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
      nonce | app_data
    ] =
      public_input |> :binary.bin_to_list() |> Cairo.get_output()

    %__MODULE__{
      tag: tag |> :binary.list_to_bin(),
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
      app_data: app_data |> Enum.map(&:binary.list_to_bin/1)
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
