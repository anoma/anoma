defmodule Nock.Jam do
  require IEx
  require Noun
  use TypedStruct

  @type cache() :: %{Noun.t() => non_neg_integer()}

  typedstruct do
    field(:position, non_neg_integer(), default: 0)
    field(:cache, cache(), default: Map.new())
    field(:buffer, [0 | 1], default: [])
  end

  @spec jam(Noun.t()) :: Noun.noun_atom()
  def jam(noun) do
    encode(%__MODULE__{}, noun)
    |> then(fn %__MODULE__{buffer: buf} -> buf end)
    |> Enum.reverse()
    |> Nock.Bits.bit_list_to_integer()
    |> Noun.atom_integer_to_binary()
  end

  @spec encode(t(), Noun.t()) :: t()
  def encode(jam_env, noun) do
    case fetch_cache_noun(jam_env, noun) do
      {:ok, position} -> handle_back(jam_env, position, noun)
      :error -> jam_env |> cache_noun(noun) |> write(noun)
    end
  end

  @spec cache_noun(t(), Noun.t()) :: t()
  def cache_noun(env = %__MODULE__{}, noun) do
    %__MODULE__{
      env
      | cache: Map.put(env.cache, Noun.normalize_noun(noun), env.position)
    }
  end

  @spec fetch_cache_noun(t(), Noun.t()) :: {:ok, non_neg_integer()} | :error
  def fetch_cache_noun(env, noun) do
    Map.fetch(env.cache, Noun.normalize_noun(noun))
  end

  @spec handle_back(t(), non_neg_integer(), Noun.t()) :: t()
  def handle_back(env, position, noun) when Noun.is_noun_cell(noun) do
    write_back_ref(env, position)
  end

  def handle_back(env, position, noun) when Noun.is_noun_atom(noun) do
    # Ensure we can do arithmetic on it!
    noun = Noun.atom_binary_to_integer(noun)
    position_length = Nock.Bits.num_bits(position, 0)
    atom_length = Nock.Bits.num_bits(noun, 0)

    if atom_length <= position_length do
      write(env, noun)
    else
      write_back_ref(env, position)
    end
  end

  @spec write(t(), Noun.t()) :: t()
  def write(env, atom) when Noun.is_noun_atom(atom) do
    # Ensure we can do arithmetic on it!
    atom = Noun.atom_binary_to_integer(atom)

    env
    |> write_to_env([0])
    |> write_length(Nock.Bits.num_bits(atom, 0))
    |> write_atom(atom)
  end

  def write(env, x = [hd | tl]) when Noun.is_noun_cell(x) do
    env
    # We reverse the tag it's [1,0] buut since we will reverse, the
    # chunk has to be reversed
    |> write_to_env([0, 1])
    |> encode(hd)
    |> encode(tl)
  end

  @spec write_length(t(), non_neg_integer()) :: t()
  def write_length(env, len_atom) do
    bit_width_of_len = Nock.Bits.num_bits(len_atom, 0)
    # We are writing [0,…ₙ₋₁,0,1] to the buffer
    # We are encoding the length of the length of the atom that we are
    # going to write
    env = write_to_env(env, [1 | List.duplicate(0, bit_width_of_len)])

    if bit_width_of_len == 0 do
      env
    else
      # we chop off the MSB
      # Our encoding is the LSB, buuuut since we have to write in
      # reverse, and cut off the first
      len_atom
      |> Nock.Bits.integer_to_bits()
      |> Enum.reverse()
      |> tl()
      |> then(&write_to_env(env, &1))
    end
  end

  @spec write_atom(t(), Noun.noun_atom()) :: t()
  def write_atom(env, atom) do
    write_to_env(env, atom |> Nock.Bits.integer_to_bits() |> Enum.reverse())
  end

  @spec write_back_ref(t(), non_neg_integer()) :: t()
  def write_back_ref(env, position) do
    env
    |> write_to_env([1, 1])
    |> write_length(Nock.Bits.num_bits(position, 0))
    |> write_atom(position)
  end

  defp write_to_env(env = %__MODULE__{buffer: buf, position: pos}, to_add) do
    %__MODULE__{env | buffer: to_add ++ buf, position: pos + length(to_add)}
  end
end
