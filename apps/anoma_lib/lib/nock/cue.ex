defmodule Nock.Cue do
  use TypedStruct
  import Bitwise

  typedstruct do
    field(:position, non_neg_integer(), default: 0)
    field(:cache, %{non_neg_integer() => Noun.t()}, default: Map.new())
  end

  @spec cue(Noun.noun_atom()) :: {:ok, Noun.t()} | :error
  def cue(number) do
    try do
      {:ok, cue!(number)}
    rescue
      _ -> :error
    end
  end

  @spec cue!(Noun.noun_atom()) :: Noun.t()
  def cue!(jammed) when is_binary(jammed) do
    jammed |> parse() |> elem(1)
  end

  @spec cue!(Noun.noun_atom()) :: Noun.t()
  def cue!(jammed) when is_integer(jammed) do
    jammed |> Noun.atom_integer_to_binary() |> cue!()
  end

  def parse(xs) do
    parse(Nock.Bits.reverse_all(xs), %__MODULE__{
      cache: Map.new(),
      position: 0
    })
  end

  # May be a bit too slow
  # Atom Case
  def parse(<<0::1, 1::1, original::bitstring>>, env = %__MODULE__{}) do
    {original, <<0>>,
     %__MODULE__{
       env
       | cache: Map.put(env.cache, env.position, <<0>>),
         position: env.position + 2
     }}
  end

  def parse(<<0::1, original::bitstring>>, env = %__MODULE__{}) do
    {atom, stream} = parse_single_atom(original)

    {stream, atom,
     %__MODULE__{
       env
       | cache: Map.put(env.cache, env.position, atom),
         position: env.position + 1 + bit_size(original) - bit_size(stream)
     }}
  end

  # Cell Case
  def parse(<<1::1, 0::1, stream::bitstring>>, oenv = %__MODULE__{}) do
    {stream, noun_1, env} =
      parse(stream, %__MODULE__{oenv | position: oenv.position + 2})

    {stream, noun_2, env} = parse(stream, env)
    cell = [noun_1 | noun_2]

    {stream, cell,
     %__MODULE__{env | cache: Map.put(env.cache, oenv.position, cell)}}
  end

  def parse(<<1::1, 1::1, original::bitstring>>, env = %__MODULE__{}) do
    {backref_location, stream} = parse_single_atom(original)

    {stream,
     Map.fetch!(env.cache, backref_location |> Noun.atom_binary_to_integer()),
     %__MODULE__{
       env
       | position: env.position + 2 + bit_size(original) - bit_size(stream)
     }}
  end

  def parse_single_atom(original) do
    {bit_width_of_size, stream} = tag_size(original, 0)

    # We remove 1 off the size, as in the Jam encoding, the MSB is
    # chopped off therefore if we had a bit size of say 4, then it'd
    # be represented hypothetically like this:
    # b₁, b₂, b₃ ∈ 𝔹
    # and where tag-size is 0…01, then we have
    # tag-size, b₁, b₂, b₃
    # Where the last 1 of the tag size represents the missing 4th bit
    # This saves 1 bit in the encoding to not store it like this:
    # tag-size, b₁, b₂, b₃, 1

    {stream, size_of_atom} = parse_num_bits(stream, bit_width_of_size - 1)
    # this number has been reversed!, reverse it back!

    {stream, number} =
      parse_num_bits(
        stream,
        Nock.Bits.to_number(<<size_of_atom::bitstring, 0::1>>) +
          (1 <<< (bit_width_of_size - 1))
      )

    atom = number |> Nock.Bits.pad_to_byte() |> Nock.Bits.reverse_all()
    {atom, stream}
  end

  def tag_size(<<0::1, rest::bitstring>>, counter) do
    tag_size(rest, counter + 1)
  end

  def tag_size(<<1::1, rest::bitstring>>, counter) do
    {counter, rest}
  end

  @doc """
  Parses the number of bits from the stream, denoted by the counter

  Note we do optimal accumulator building
  """
  def parse_num_bits(stream, counter) do
    parse_num_bits(stream, counter, <<>>)
  end

  def parse_num_bits(stream, 0, acc) do
    {stream, acc}
  end

  def parse_num_bits(<<x::1, stream::bitstring>>, counter, acc) do
    parse_num_bits(stream, counter - 1, <<acc::bitstring, x::1>>)
  end
end
