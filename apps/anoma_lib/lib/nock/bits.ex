defmodule Nock.Bits do
  import Bitwise

  @spec num_bits(Noun.noun_atom(), non_neg_integer()) ::
          non_neg_integer()
  def num_bits(n, block_size) when is_binary(n) do
    n |> Noun.atom_binary_to_integer() |> num_bits(block_size)
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

  def match_byte_reverse(
        <<x_1::1, x_2::1, x_3::1, x_4::1, x_5::1, x_6::1, x_7::1, x_8::1>>
      ) do
    <<x_8::1, x_7::1, x_6::1, x_5::1, x_4::1, x_3::1, x_2::1, x_1::1>>
  end

  @spec to_number(bitstring(), non_neg_integer(), non_neg_integer()) :: non_neg_integer()
  def to_number(stream) do
    to_number(stream, 0, 0)
  end

  def to_number(<<x::1, stream::bitstring>>, acc, position) do
    to_number(stream, acc + (x <<< position), position + 1)
  end

  def to_number(<<>>, acc, _position) do
    acc
  end

  @spec pad_to_byte(bitstring()) :: binary()
  def pad_to_byte(stream) do
    pad_to_byte(stream, 8 - band(bit_size(stream), 0b111))
  end

  def pad_to_byte(stream, 0) do
    stream
  end

  def pad_to_byte(stream, counter) do
    pad_to_byte(<<stream :: bitstring, 0 :: 1>> , counter - 1)
  end

  def reverse_all(xs) do
    reverse_all(xs, <<>>)
  end

  def reverse_all(<<>>, acc) do
    acc
  end

  def reverse_all(<<x::bits-size(8), xs::binary>>, acc) do
    reverse_all(
      xs,
      <<acc::binary, Nock.Bits.match_byte_reverse(x)::bits-size(8)>>
    )
  end
end
