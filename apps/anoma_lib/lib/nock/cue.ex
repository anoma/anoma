defmodule Nock.Cue do
  use TypedStruct

  import Bitwise

  @type cue_tag() :: :atom | :cell | :backref

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
  def cue!(number) do
    number
    |> Noun.Bits.integer_to_bits()
    |> parse()
    |> elem(0)
  end

  # consume atom basically
  # we should have a version that parses forward by n
  @spec rub_no_index([0 | 1], t()) :: {non_neg_integer(), list(), t()}
  def rub_no_index(bit_stream, env) do
    {bit_width_of_length, stream, env} = bit_width_tag(bit_stream, env)
    do_rub_no_index(bit_width_of_length, stream, env)
  end

  defp do_rub_no_index(0, stream, env) do
    {0, stream, env}
  end

  defp do_rub_no_index(bit_width_of_length, stream, env) do
    # length is the most significant bit ommited
    bit_width_left = bit_width_of_length - 1

    {length_decode, new_stream} = Enum.split(stream, bit_width_left)

    # this number is missing the MSB, so we need to or it with the mask
    length_parsed_number =
      Noun.Bits.bit_list_to_integer(length_decode)

    masked_number = 1 <<< bit_width_left

    length = bor(masked_number, length_parsed_number)

    {integer_decode, new_stream} = Enum.split(new_stream, length)

    atom =
      Noun.Bits.bit_list_to_integer(integer_decode)

    {atom, new_stream,
     %__MODULE__{env | position: env.position + length + bit_width_left}}
  end

  @spec parse([0 | 1]) :: {Noun.t(), [0 | 1], t()}
  @spec parse([0 | 1], t()) :: {Noun.t(), [0 | 1], t()}
  def parse(bit_stream) do
    parse(bit_stream, %__MODULE__{cache: Map.new(), position: 0})
  end

  def parse(bit_stream, cue_env) do
    parse_single(cue_tag(bit_stream, cue_env), cue_env.position)
  end

  @spec parse_single({cue_tag(), [0 | 1], t()}, non_neg_integer()) ::
          {Noun.t(), [0 | 1], t()}
  def parse_single({:atom, binary, env}, pos_before_parse) do
    {atom, binary, env} = rub_no_index(binary, env)

    {atom, binary,
     %__MODULE__{env | cache: Map.put(env.cache, pos_before_parse, atom)}}
  end

  def parse_single({:cell, binary, env}, pos_before_parse) do
    {head, binary, env} = parse(binary, env)
    {tail, binary, env} = parse(binary, env)
    res = [head | tail]

    {res, binary,
     %__MODULE__{env | cache: Map.put(env.cache, pos_before_parse, res)}}
  end

  def parse_single({:backref, binary, env}, _pos_before_parse) do
    {backref, binary, env} = rub_no_index(binary, env)
    # backref should relate to a previous position we cached
    {Map.fetch!(env.cache, backref), binary, env}
  end

  @spec cue_tag(list(), t()) :: {cue_tag(), [0 | 1], t()}
  def cue_tag([1, 0 | any], env) do
    {:cell, any, %__MODULE__{env | position: env.position + 2}}
  end

  def cue_tag([0 | any], env) do
    {:atom, any, %__MODULE__{env | position: env.position + 1}}
  end

  def cue_tag([1, 1 | any], env) do
    {:backref, any, %__MODULE__{env | position: env.position + 2}}
  end

  @spec bit_width_tag([1 | 0], t()) :: {non_neg_integer(), [1 | 0], t()}
  @spec bit_width_tag([1 | 0], t(), non_neg_integer()) ::
          {non_neg_integer(), [1 | 0], t()}
  def bit_width_tag(binary, env) do
    bit_width_tag(binary, env, 0)
  end

  def bit_width_tag([0 | any], env, counter) do
    bit_width_tag(any, env, counter + 1)
  end

  def bit_width_tag([1 | any], env, counter) do
    # The MSR of the length is always 1 and thus we omit that bit
    # if we start with 1, it ought to be 0
    {counter, any, %__MODULE__{env | position: env.position + counter + 1}}
  end
end
