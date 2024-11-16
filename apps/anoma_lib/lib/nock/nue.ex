defmodule Nue do
  def cue(bytes) do
    # we could store binary atoms the other way to not do this.
    # if we did, our strings would print reversed.
    bytes = byte_order_flip(bytes)

    # now, trim leading zeroes and turn it into a bitstring rather than
    # a binary made of octets.
    padded_size = bit_size(bytes)
    real_size = real_size(bytes)

    <<0::size(padded_size - real_size), bits::size(real_size)-bitstring>> =
      bytes

    {result, <<>>, _, _} = cue_bits(bits, real_size)
    result
  end

  def cue_bits(bits, size, offset \\ 0, cache \\ %{}) do
    case bits do
      # special case for atom 0, which is 0-length.
      # this does comply with the format but would require encoding a
      # bit string of -1 bits in length. the special case is best.
      <<rest::size(size - 2)-bitstring, 1::size(1), 0::size(1)>> ->
        {<<>>, rest, offset + 2, Map.put(cache, offset, <<>>)}

      # atom: encoded in a mildly complicated way.
      <<rest::size(size - 1)-bitstring, 0::size(1)>> ->
        cue_atom(rest, size - 1, offset, cache, 1)

      # cell: just the head, followed by the tail.
      <<rest::size(size - 2)-bitstring, 0::size(1), 1::size(1)>> ->
        {head, continuation_1, offset_1, cache_1} =
          cue_bits(rest, size - 2, offset + 2, cache)

        {tail, continuation_2, offset_2, cache_2} =
          cue_bits(
            continuation_1,
            bit_size(continuation_1),
            offset_1,
            cache_1
          )

        cell = [head | tail]
        {cell, continuation_2, offset_2, Map.put(cache_2, offset, cell)}

      # backref: fetch from the cache
      <<rest::size(size - 2)-bitstring, 1::size(1), 1::size(1)>> ->
        {backref_key, continuation, new_offset, _unused_new_cache} =
          cue_atom(rest, size - 2, offset + 2, cache, 2)

        {Map.fetch!(cache, :binary.decode_unsigned(backref_key)),
         continuation, new_offset, cache}
    end
  end

  def cue_atom(bits, size, offset, cache, tag_bits) do
    # the length of the length is stored in unary; as zeroes terminated by a 1.
    length_of_length = count_trailing_zeros(bits, size)
    length_of_length_of_length = length_of_length + 1

    # having found the length of the length, advance the bitstream.
    # shadowing bits and size here.
    size = size - length_of_length_of_length
    <<bits::size(size)-bitstring, _::size(length_of_length_of_length)>> = bits

    # now we can read the length, which is actually one shorter, the most
    # significant bit is always 1, and not stored.
    # advance the bitstream by shadowing again.
    size = size - (length_of_length - 1)

    <<bits::size(size)-bitstring,
      length::size(length_of_length - 1)-bitstring>> = bits

    <<length::size(length_of_length)-integer>> =
      <<1::size(1), length::bitstring>>

    # now we have the actual length and can read that many bits off the bitstream.
    size = size - length
    <<rest::size(size)-bitstring, atom::size(length)-bitstring>> = bits

    # now pad the atom back into a binary.
    # this throws away information; alas!
    # maybe we should just support all bitstrings???
    atom_bits = bit_size(atom)
    padding_bits = Kernel.rem(8 - Kernel.rem(atom_bits, 8), 8)
    atom = <<0::size(padding_bits), atom::bitstring>>

    # at last, return the atom and remaining bitstream.
    # got to flip it (on a byte level) here given how we store them.
    final_atom = atom |> byte_order_flip()

    {final_atom, rest, length + 2 * length_of_length + tag_bits,
     Map.put(cache, offset, final_atom)}
  end

  def count_trailing_zeros(bits, size) do
    case bits do
      <<rest::size(size - 1)-bitstring, 0::1>> ->
        1 + count_trailing_zeros(rest, size - 1)

      <<_::size(size - 1), 1::1>> ->
        0
    end
  end

  defp byte_order_flip(bytes) do
    bytes
    |> :binary.decode_unsigned()
    |> :binary.encode_unsigned(:little)
  end

  defp real_size(<<0::1, rest::bitstring>>) do
    real_size(rest)
  end

  defp real_size(bits = <<1::1, _::bitstring>>) do
    bit_size(bits)
  end
end
