defmodule Nam do
  require Noun

  use TypedStruct

  @type cache() :: %{Noun.t() => non_neg_integer()}

  typedstruct do
    field(:position, non_neg_integer(), default: 0)
    field(:cache, cache(), default: Map.new())
  end

  @spec jam(Noun.t()) :: Noun.noun_atom()
  def jam(noun) do
    Noun.normalize_noun(noun)
    |> encode(%__MODULE__{}, <<>>)
    |> elem(0)
    |> nearest_byte()
    |> Nock.Bits.byte_order_big_to_little()
  end

  def encode(noun, env = %__MODULE__{}, bits) do
    case Map.fetch(env.cache, noun) do
      {:ok, position} -> backref(position, noun, env, bits)
      :error -> write(noun, cache_noun(noun, env), bits)
    end
  end

  def backref(position, cell, env, bits) when Noun.is_noun_cell(cell) do
    write_backref(position, env, bits)
  end

  def backref(position, atom, env, bits) when is_bitstring(atom) do
    position_length = real_size(:binary.encode_unsigned(position))
    atom_length = real_size(Nock.Bits.byte_order_little_to_big(atom))

    if atom_length <= position_length do
      write(atom, env, bits)
    else
      write_backref(position, env, bits)
    end
  end

  def write_backref(position, env, bits) do
    {acc, offset} = write_atom(Noun.atom_integer_to_binary(position), bits)

    {<<acc::bitstring, 1::1, 1::1>>,
     %__MODULE__{env | position: env.position + offset + 2}}
  end

  def write(atom, env = %__MODULE__{}, bits) when is_bitstring(atom) do
    {acc, offset} = write_atom(atom, bits)

    {<<acc::bitstring, 0::1>>,
     %__MODULE__{env | position: env.position + offset + 1}}
  end

  def write(cell = [hd | tl], env, bits) when Noun.is_noun_cell(cell) do
    # We have to do this in an odd way, going hd first, for offset
    # calculations...

    {bits_hd, env_1} =
      encode(hd, %__MODULE__{env | position: env.position + 2}, <<>>)

    # We now doe the tl which should be the first
    {bits_tl, env_2} =
      encode(tl, env_1, <<>>)

    # More copying than I'd like...
    {<<bits::bitstring, bits_tl::bitstring, bits_hd::bitstring, 0::1, 1::1>>,
     env_2}
  end

  def write_atom(<<>>, bits) do
    {<<bits::bitstring, 1::1>>, 1}
  end

  def write_atom(atom, bits) do
    atom_flipped = Nock.Bits.byte_order_little_to_big(atom)

    # The actual length of the atom, with the leading 0's stripped
    length_of_atom = real_size(atom_flipped)

    binary_length_of_atom = :binary.encode_unsigned(length_of_atom)

    # This is the length of the length
    length_of_length_of_atom =
      real_size(binary_length_of_atom)

    # How many padding bits of the atom we need to strip
    padding_bits_of_atom = padding(length_of_atom)

    # How many padding bits off the length_of_atom we need to strip
    padding_bits_of_length = padding(length_of_length_of_atom)

    # We strip the leading 0's along with the MSB
    <<0::size(padding_bits_of_length), 1::1, length_atom::bitstring>> =
      binary_length_of_atom

    # We just strip the padding bits
    <<0::size(padding_bits_of_atom), atom::bitstring>> = atom_flipped

    {<<bits::bitstring, atom::bitstring, length_atom::bitstring, 1::1,
       0::size(length_of_length_of_atom)>>,
     bit_size(atom) + bit_size(length_atom) + 1 + length_of_length_of_atom}
  end

  @spec cache_noun(Noun.t(), t()) :: t()
  def cache_noun(noun, env = %__MODULE__{}) do
    %__MODULE__{env | cache: Map.put(env.cache, noun, env.position)}
  end

  def real_size(<<>>) do
    0
  end

  def real_size(<<0::1, rest::bitstring>>) do
    real_size(rest)
  end

  def real_size(bits = <<1::1, _::bitstring>>) do
    bit_size(bits)
  end

  def padding(size) do
    rem(8 - rem(size, 8), 8)
  end

  def nearest_byte(bitstring) do
    <<0::size(padding(bit_size(bitstring))), bitstring::bitstring>>
  end
end
