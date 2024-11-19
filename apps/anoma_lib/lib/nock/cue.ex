defmodule Nock.Cue do
  @moduledoc """
  I am a module implementing cue deserialization functionality for Nock.

  ### Public API

  I have the following public functionality:

  - `cue/1`
  - `cue!/1`
  """

  @typedoc """
  I am the type of the cue cache, that is, I store the already decoded
  nouns as values to their encodings in a map for optimization.
  """

  @type cue_cache() :: %{non_neg_integer() => Noun.t()}

  @doc """
  I am the cue function.

  I execute `cue!` in a try environment and return the result wrapped in
  `{:ok, result}` upon success. Return `:error` otherwise.

  See `cue!/1` for more information.
  """

  @spec cue(binary()) :: {:ok, Noun.t()} | :error
  def cue(number) do
    try do
      {:ok, cue!(number)}
    rescue
      _ -> :error
    end
  end

  @doc """
  I am the cue! function.

  Given jammed input, I first change the endianness of the input, unpad the extra 0es
  inserted and proceed via the usual deserialization of Nock terms.
  """

  @spec cue!(binary()) :: Noun.t()
  def cue!(bytes) do
    # we could store binary atoms the other way to not do this.
    # if we did, our strings would print reversed.
    bytes = Nock.Bits.byte_order_little_to_big(bytes)

    # now, trim leading zeroes and turn it into a bitstring rather than
    # a binary made of octets.
    {bits, real_size} = unpad_from_binary(bytes)

    # we expect to consume real_size bits and have nothing left over.
    {result, <<>>, ^real_size, _} = cue_bits(bits, real_size)
    result
  end

  @spec cue_bits(
          bitstring(),
          non_neg_integer(),
          non_neg_integer(),
          cue_cache()
        ) ::
          {Noun.t(), bitstring(), non_neg_integer(), cue_cache()}
  defp cue_bits(bits, size, offset \\ 0, cache \\ %{}) do
    case bits do
      # special case for atom 0, which is 0-length.
      # this does comply with the format but would require encoding a
      # bit string of -1 bits in length. the special case is best.
      <<rest::size(size - 2)-bitstring, 1::size(1), 0::size(1)>> ->
        {<<>>, rest, offset + 2, Map.put(cache, offset, <<>>)}

      # atom: encoded in a mildly complicated way. 1 tag bit.
      <<rest::size(size - 1)-bitstring, 0::size(1)>> ->
        cue_atom(rest, size - 1, offset, cache, 1)

      # cell: after the 2 tag bits, just the head, followed by the tail.
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

      # backref: fetch from the cache. encoded the same way as atoms,
      # but there's 2 tag bits so we pass a 2 there. while cue_atom
      # returns a new cache for simplicity, the number isn't actually
      # an encoded atom, so we ignore the updated cache value.
      <<rest::size(size - 2)-bitstring, 1::size(1), 1::size(1)>> ->
        {backref_key, continuation, new_offset, _unused_new_cache} =
          cue_atom(rest, size - 2, offset, cache, 2)

        {Map.fetch!(cache, :binary.decode_unsigned(backref_key, :little)),
         continuation, new_offset, cache}
    end
  end

  @spec cue_atom(
          bitstring(),
          non_neg_integer(),
          non_neg_integer(),
          cue_cache(),
          1 | 2
        ) ::
          {Noun.t(), bitstring(), non_neg_integer(), cue_cache()}
  defp cue_atom(bits, size, offset, cache, tag_bits) do
    # the length of the length is stored in unary; as zeroes terminated by a 1.
    length_of_length = count_trailing_zeros(bits, size)
    length_of_length_of_length = length_of_length + 1

    # having found the length of the length, advance the bitstream.
    # shadowing bits and size here to accomplish that.
    size = size - length_of_length_of_length
    <<bits::size(size)-bitstring, _::size(length_of_length_of_length)>> = bits

    # now we can read the length, which is actually one bit shorter;
    # the most significant bit is always 1, and not stored.
    # advance the bitstream by shadowing bits and size again.
    size = size - (length_of_length - 1)

    <<bits::size(size)-bitstring,
      length::size(length_of_length - 1)-bitstring>> = bits

    <<length::size(length_of_length)-integer>> =
      <<1::size(1), length::bitstring>>

    # now we have the actual length and can read that many bits off the
    # bitstream. shadowing bits and length once more.
    size = size - length
    <<bits::size(size)-bitstring, atom::size(length)-bitstring>> = bits

    # now pad the atom back into a binary.
    # this throws away information; alas!
    # maybe we should just support all bitstrings???
    padded_atom = pad_to_binary(atom)

    # at last, return the atom and remaining bitstream.
    # got to flip it (on a byte level) here given how we store them.
    final_atom = padded_atom |> Nock.Bits.byte_order_big_to_little()

    bits_consumed = length + 2 * length_of_length + tag_bits

    {final_atom, bits, offset + bits_consumed,
     Map.put(cache, offset, final_atom)}
  end

  @spec pad_to_binary(bitstring()) :: binary()
  def pad_to_binary(bits) do
    padding_bits = Kernel.rem(8 - Kernel.rem(bit_size(bits), 8), 8)
    <<0::size(padding_bits), bits::bitstring>>
  end

  @spec unpad_from_binary(binary()) :: {bitstring(), non_neg_integer()}
  def unpad_from_binary(bytes) do
    padded_size = bit_size(bytes)
    real_size = real_size(bytes)

    <<0::size(padded_size - real_size), bits::size(real_size)-bitstring>> =
      bytes

    {bits, real_size}
  end

  @spec count_trailing_zeros(bitstring(), non_neg_integer()) ::
          non_neg_integer()
  defp count_trailing_zeros(bits, size, acc \\ 0) do
    case bits do
      <<>> ->
        acc

      <<_::size(size - 1), 1::1>> ->
        acc

      <<rest::size(size - 1)-bitstring, 0::1>> ->
        count_trailing_zeros(rest, size - 1, acc + 1)
    end
  end

  @spec real_size(bitstring()) :: non_neg_integer()
  defp real_size(bits) do
    bit_size(bits) - count_leading_zeros(bits)
  end

  @spec count_leading_zeros(bitstring()) :: non_neg_integer()
  defp count_leading_zeros(bits, acc \\ 0) do
    case bits do
      <<>> ->
        acc

      <<1::1, _::bitstring>> ->
        acc

      # hereinafter, unroll the cases we actually expect;
      # this is slightly faster and it's a hotspot

      <<0::7, rest::bitstring>> ->
        count_leading_zeros(rest, acc + 7)

      <<0::6, rest::bitstring>> ->
        count_leading_zeros(rest, acc + 6)

      <<0::5, rest::bitstring>> ->
        count_leading_zeros(rest, acc + 5)

      <<0::4, rest::bitstring>> ->
        count_leading_zeros(rest, acc + 4)

      <<0::3, rest::bitstring>> ->
        count_leading_zeros(rest, acc + 3)

      <<0::2, rest::bitstring>> ->
        count_leading_zeros(rest, acc + 2)

      <<0::1, rest::bitstring>> ->
        count_leading_zeros(rest, acc + 1)
    end
  end
end
