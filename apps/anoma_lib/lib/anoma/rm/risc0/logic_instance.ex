
defmodule Anoma.RM.Risc0.LogicInstance do
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

  @spec from_receipt(binary()) :: t()
  def from_receipt(receipt) do
    # call Risc0 API to get the logic instance
    [
      tag,
      root,
      mac,
      pk_x,
      pk_y,
      nonce,
      cipher_text_elem0,
      cipher_text_elem1,
      cipher_text_elem2,
      cipher_text_elem3,
      cipher_text_elem4,
      cipher_text_elem5,
      cipher_text_elem6,
      cipher_text_elem7,
      cipher_text_elem8,
      cipher_text_elem9 | app_data
    ] =
      receipt |> :binary.bin_to_list() |> Risc0.get_logic_instance()

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
  def decrypt(cipher, sk) do
    cipher
    |> Enum.map(&:binary.bin_to_list/1)
    |> Risc0.decrypt(:binary.bin_to_list(sk))
    |> Enum.map(&:binary.list_to_bin/1)
  end
end
