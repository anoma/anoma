defmodule Nue do
  def cue(bytes) do
    padded_size = bit_size(bytes)
    real_size = real_size(bytes)

    <<0::size(padded_size - real_size), bits::size(real_size)-bitstring>> =
      bytes

    {result, <<>>} = cue_bits(bits, real_size)
    result
  end

  def cue_bits(bits, size) do
    case bits do
      <<rest::size(size - 2)-bitstring, 1::size(1), 0::size(1)>> ->
        {<<>>, rest}

      <<rest::size(size - 2)-bitstring, 0::size(1), 1::size(1)>> ->
        {head, continuation_1} = cue_bits(rest, size - 2)

        {tail, continuation_2} =
          cue_bits(continuation_1, bit_size(continuation_1))

        {[head | tail], continuation_2}
    end
  end

  defp real_size(<<0::1, rest::bitstring>>) do
    real_size(rest)
  end

  defp real_size(bits = <<1::1, _::bitstring>>) do
    bit_size(bits)
  end
end
