defmodule Anoma.CairoResource.Logic.Instance do
  @moduledoc """
  I represent the resource logic's instance.
  """

  alias __MODULE__
  use TypedStruct

  typedstruct enforce: true do
    # nullifier of input resource or commitment of output resource
    field(:tag, <<_::256>>, default: <<0::256>>)
    # a flag that tells the logic if the resource is consumed or created
    field(:is_consumed, <<_::256>>, default: <<0::256>>)
    # The merkle root of resources in current action(execution context)
    field(:root, <<_::256>>, default: <<0::256>>)
    # Ciphertext
    field(:cipher, list(<<_::256>>), default: [])
    # app_data: list (BitString, DeletionCriterion)
    field(:app_data, list({<<_::256>>, <<_::256>>}), default: [])
  end

  @spec to_instance(list(byte())) :: t()
  def to_instance(public_input) do
    # call cairo api to get output bytes
    output = public_input |> Cairo.get_output()

    unless is_list(output) and length(output) >= 17 do
      raise ArgumentError, "Invalid output from Cairo.get_output/1"
    end

    [
      tag,
      is_consumed,
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
    ] = output

    %Instance{
      tag: tag |> :binary.list_to_bin(),
      is_consumed: is_consumed |> :binary.list_to_bin(),
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
      app_data:
        app_data |> Enum.map(&:binary.list_to_bin/1) |> Enum.chunk_every(2)
    }
  end

  @spec decrypt(list(binary()), binary()) ::
          {:ok, list(binary())} | {:error, term()}
  def decrypt(cipher, sk) do
    sk_bin_list = :binary.bin_to_list(sk)

    case cipher
         |> Enum.map(&:binary.bin_to_list/1)
         |> Cairo.decrypt(sk_bin_list) do
      {:error, reason} ->
        {:error, reason}

      plain_text ->
        {:ok, plain_text |> Enum.map(&:binary.list_to_bin/1)}
    end
  end
end
