defmodule Nock.Bits do
  import Bitwise

  @spec num_bits(Noun.noun_atom(), non_neg_integer()) ::
          non_neg_integer()
  def num_bits(n, block_size) when is_binary(n) do
    size = block_size |> Noun.atom_binary_to_integer()
    n |> Noun.atom_binary_to_integer() |> num_bits(size)
  end

  def num_bits(n, block_size) when n >= 0 do
    num_bits(n, 1 <<< block_size, 0)
  end

  defp num_bits(0, _, acc), do: acc

  defp num_bits(n, block_size, acc) do
    num_bits(n >>> block_size, block_size, acc + 1)
  end

  @spec bit_list_to_integer([0 | 1]) :: non_neg_integer()
  def bit_list_to_integer(bits) do
    bits
    |> Enum.with_index()
    |> Enum.reduce(0, fn {bit, index}, acc -> acc + (bit <<< index) end)
  end

  @spec integer_to_bits(Noun.noun_atom()) :: [0 | 1]
  def integer_to_bits(number) do
    number
    |> Noun.atom_integer_to_binary()
    |> :binary.bin_to_list()
    |> Enum.flat_map(&byte_to_bits/1)
    |> Enum.take(num_bits(Noun.atom_binary_to_integer(number), 0))
  end

  defp byte_to_bits(byte) do
    Enum.map(0..7, fn shift -> byte >>> shift &&& 1 end)
  end

  @doc """
  I take in little endian encoded bytes and switch the encoding to big
  endian.
  """

  @spec byte_order_little_to_big(binary()) :: binary()
  def byte_order_little_to_big(bytes) do
    bytes
    |> :binary.decode_unsigned(:little)
    |> :binary.encode_unsigned()
  end

  @doc """
  I take in big endian encoded bytes and switch the encoding to little
  endian.
  """

  @spec byte_order_big_to_little(binary()) :: binary()
  def byte_order_big_to_little(bytes) do
    bytes
    |> :binary.decode_unsigned()
    |> :binary.encode_unsigned(:little)
  end
end
