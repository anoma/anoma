defmodule Noun.Jam do
  @moduledoc """
  I am the implementation of Nock de/serialization functions, cue and jam.

  Jam is optimized for small output size; cue is optimized for speed.
  (At least in theory; this implementation is not claiming perfection.)

  Any improvements made here should neither make jam outputs larger nor
  make cue slower. Additionally, cue compatibility across all existing
  implementations is a hard requirement.

  This implementation, unlike others, only emits references in jam if
  they would not make the output larger; existing implementations have
  a bug in calculating this. However, all existing implementations of
  cue are compatible with both encoders' output. Correctly calculating
  this makes outputs significantly smaller on large nouns.

  Because we're going "backwards" in the Erlang runtime system sense,
  there's going to be some excessive copying. Cueing shouldn't need too
  much, except for alignment reasons, I think.

  ### Public API

  I have the following public functionality:

  - `jam/1`
  - `cue/1`
  - `cue!/1`
  """

  require Noun

  @typedoc """
  I am the jam cache type. I store the reference alongside with its size.
  """

  @type jam_cache() :: %{Noun.t() => {bitstring(), non_neg_integer()}}

  @typedoc """
  I am the type of the cue cache, that is, I store the already decoded
  nouns as values to their encodings in a map for optimization.
  """

  @type cue_cache() :: %{non_neg_integer() => Noun.t()}

  ############################################################
  #                       Exceptions                         #
  ############################################################

  defmodule JamError do
    @moduledoc """
    I am the exception thrown by jam/1 when it encounters a non-noun
    which it is incapable of encoding.
    """

    defexception [:message]

    @impl true
    def exception(purported_noun) do
      %JamError{
        message: "can't encode non-noun term: #{inspect(purported_noun)}"
      }
    end
  end

  defmodule CueError do
    @moduledoc """
    I am the exception thrown by cue!/1 when it cannot decode further.
    """

    defexception [:message]
  end

  ############################################################
  #                          Jam                             #
  ############################################################

  @doc """
  I am the jam function.

  I serialize any Nock noun into binary format. My inverse is `cue!/1`
  """

  @spec jam(Noun.t()) :: binary()
  def jam(noun) do
    {bits, _cache, offset} = jam_inner(noun)
    # sanity check
    ^offset = bit_size(bits)
    bits |> pad_to_binary() |> Nock.Bits.byte_order_big_to_little()
  end

  @spec jam_inner(Noun.t(), jam_cache(), non_neg_integer()) ::
          {bitstring(), jam_cache(), non_neg_integer()}
  defp jam_inner(noun, cache \\ %{}, offset \\ 0) do
    # this could be Map.get_lazy if the offset tracking
    # didn't make using that awkward.
    with {backref, backref_size} <- Map.get(cache, noun) do
      {backref, cache, offset + backref_size}
    else
      _ ->
        # cases where we will obviously never store a backref
        {backref, backref_size} =
          if offset == 0 or Noun.is_noun_zero(noun) do
            {nil, nil}
          else
            # compute a backref to our offset here first
            {offset_bits, offset_bits_size} = integer_to_bits(offset)

            {encoded_offset, encoded_offset_size} =
              jam_atom_encode(offset_bits, offset_bits_size)

            backref = <<encoded_offset::bitstring, 1::1, 1::1>>
            backref_size = encoded_offset_size + 2
            # sanity check
            ^backref_size = bit_size(backref)
            {backref, backref_size}
          end

        case noun do
          [head | tail] ->
            {jammed_head, cache_after_head, offset_after_head} =
              jam_inner(head, cache, offset + 2)

            {jammed_tail, new_cache, new_offset} =
              jam_inner(tail, cache_after_head, offset_after_head)

            encoded_cell =
              <<jammed_tail::bitstring, jammed_head::bitstring, 0::1, 1::1>>

            encoded_cell_size = bit_size(encoded_cell)

            # if the backref to here would be no larger than the encoding,
            # put it in the cache.
            maybe_updated_cache =
              if backref_size <= encoded_cell_size do
                Map.put(new_cache, noun, {backref, backref_size})
              else
                new_cache
              end

            {encoded_cell, maybe_updated_cache, new_offset}

          zero when Noun.is_noun_zero(zero) ->
            # there is no possible backref shorter than this,
            # and 0 is not a valid backref offset since it means
            # "the entire noun we are jamming". (it would be 0b111 anyway.)
            # offset 1 would be 0b11011, 2.5x the size
            # so no cache update. 0s are never backreffed-to
            {<<1::1, 0::1>>, cache, offset + 2}

          atom when Noun.is_noun_atom(atom) ->
            {atom_bits, atom_size} =
              atom
              |> Noun.normalize_noun()
              |> Nock.Bits.byte_order_little_to_big()
              |> unpad_from_binary()

            {encoded_atom, encoded_atom_size} =
              jam_atom_encode(atom_bits, atom_size)

            # add the atom tag bit, shadowing encoded_atom and encoded_atom_size
            encoded_atom = <<encoded_atom::bitstring, 0::1>>
            encoded_atom_size = encoded_atom_size + 1

            # if the backref to here would be no larger than the encoding,
            # put it in the cache.
            maybe_updated_cache =
              if backref_size <= encoded_atom_size do
                Map.put(cache, noun, {backref, backref_size})
              else
                cache
              end

            {encoded_atom, maybe_updated_cache, offset + encoded_atom_size}

          _ ->
            raise JamError, noun
        end
    end
  end

  @spec jam_atom_encode(<<>>, 0) :: {<<_::1>>, 1}
  defp jam_atom_encode(<<>>, 0) do
    {<<1::1>>, 1}
  end

  @spec jam_atom_encode(bitstring(), non_neg_integer()) ::
          {bitstring(), non_neg_integer()}
  defp jam_atom_encode(atom_bits, atom_size) do
    {atom_size_as_bits, atom_size_of_size} = integer_to_bits(atom_size)

    <<1::1, atom_size_truncated::bitstring>> = atom_size_as_bits

    # from right to left: unary size of size,
    # atom size with leading 1 chopped off, actual atom bits
    encoded_atom =
      <<atom_bits::bitstring, atom_size_truncated::bitstring, 1::1,
        0::size(atom_size_of_size)>>

    encoded_atom_size = bit_size(encoded_atom)

    {encoded_atom, encoded_atom_size}
  end

  ############################################################
  #                          Cue                             #
  ############################################################

  @doc """
  I am the cue function.

  I execute `cue!` in a try environment and return the result wrapped in
  `{:ok, result}` upon success. Return `:error` otherwise.

  See `cue!/1` for more information.
  """

  @spec cue(binary()) :: {:ok, Noun.t()} | :error
  def cue(bytes) do
    try do
      {:ok, cue!(bytes)}
    rescue
      _ -> :error
    end
  end

  @doc """
  I am the cue! function.

  Given jammed input, I first change the endianness of the input, unpad the extra 0es
  inserted and proceed via the usual deserialization of Nock terms.

  My inverse is `jam/1`
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
    with {result, <<>>, ^real_size, _} <- cue_bits(bits, real_size) do
      result
    else
      {_, remaining, _, _} ->
        raise CueError,
              "cued complete noun with #{inspect(bit_size(remaining))} bits remaining"
    end
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
        try do
          cue_atom(rest, size - 1, offset, cache, 1)
        rescue
          _ ->
            reraise CueError,
                    "failed to decode atom at offset #{inspect(offset)}",
                    __STACKTRACE__
        end

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
          try do
            cue_atom(rest, size - 2, offset, cache, 2)
          rescue
            _ ->
              reraise CueError,
                      "failed to decode backref at offset #{inspect(offset)}",
                      __STACKTRACE__
          end

        with {:ok, referenced_noun} <-
               Map.fetch(cache, :binary.decode_unsigned(backref_key, :little)) do
          {referenced_noun, continuation, new_offset, cache}
        else
          _ ->
            raise CueError,
                  "invalid backref at offset #{inspect(offset)}"
        end

      _ ->
        raise CueError,
              "expected an atom or cell at offset: #{inspect(offset)}"
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
  defp pad_to_binary(bits) do
    padding_bits = Kernel.rem(8 - Kernel.rem(bit_size(bits), 8), 8)
    <<0::size(padding_bits), bits::bitstring>>
  end

  @spec unpad_from_binary(binary()) :: {bitstring(), non_neg_integer()}
  defp unpad_from_binary(bytes) do
    padded_size = bit_size(bytes)
    real_size = real_size(bytes)

    <<0::size(padded_size - real_size), bits::size(real_size)-bitstring>> =
      bytes

    {bits, real_size}
  end

  @spec integer_to_bits(non_neg_integer()) :: {bitstring(), non_neg_integer()}
  defp integer_to_bits(n) do
    n |> :binary.encode_unsigned(:big) |> unpad_from_binary()
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

  @spec real_size(bitstring()) :: non_neg_integer()
  defp real_size(bits) do
    bit_size(bits) - count_leading_zeros(bits)
  end
end
