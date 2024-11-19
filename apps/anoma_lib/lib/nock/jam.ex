defmodule Nock.Jam do
  @moduledoc """
  I am a module implementing jam serealization functionality for Nock.

  ### Public API

  I have the following public functionality:

  - `jam/1`
  """

  require Noun

  @typedoc """
  I am the jam cache type. I store the reference alongside with its size.
  """
  @type jam_cache() :: %{Noun.t() => {bitstring(), non_neg_integer()}}

  @doc """
  I am the jam function.

  I serialize any Nock noun into binary format. My inverse is `cue!/1`
  """

  @spec jam(Noun.t()) :: binary()
  def jam(noun) do
    {bits, _cache, offset} = jam_inner(noun)
    # sanity check
    ^offset = bit_size(bits)
    bits |> Nock.Cue.pad_to_binary() |> Nock.Bits.byte_order_big_to_little()
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
              |> Nock.Cue.unpad_from_binary()

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

  @spec integer_to_bits(non_neg_integer()) :: {bitstring(), non_neg_integer()}
  defp integer_to_bits(n) do
    n |> :binary.encode_unsigned(:big) |> Nock.Cue.unpad_from_binary()
  end
end
