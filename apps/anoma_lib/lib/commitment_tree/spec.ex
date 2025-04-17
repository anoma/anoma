defmodule CommitmentTree.Spec do
  @moduledoc """
  A specification for a commitment tree.
  """
  alias __MODULE__

  use TypedStruct

  @typedoc """
  A type of possible merkle tree hashes currently supported.
  """
  @type accumulator_hash :: :sha256 | :poseidon

  typedstruct enforce: true do
    # the (fixed) depth of the tree
    field(:depth, integer())

    # the number of children of each internal node.  need not be a power of two
    field(:splay, integer())
    # the number of bits in a commitment
    field(:key_size, integer())
    # an atom specifying the appropriate hash function to use
    field(:hash, accumulator_hash)
    # cached <<0::size(key_size)>>
    field(:key_zero, binary())

    # suffix product of a repeated splay: i.e. a cached [splay^(depth-1), splay^(depth-2), ...]; i.e., at a given level, how many leaves are covered by each child?
    field(:splay_suff_prod, list(integer()))
  end

  @spec new(integer(), integer(), integer(), accumulator_hash()) :: t()
  def new(depth, splay, key_size, hash) do
    %CommitmentTree.Spec{
      depth: depth,
      splay: splay,
      key_size: key_size,
      hash: hash,
      key_zero: <<0::size(key_size)>>,
      splay_suff_prod: Enum.map((depth - 1)..1//-1, fn i -> splay ** i end)
    }
  end

  # It's a sha256 tree spec by default
  @spec cm_tree_spec() :: CommitmentTree.Spec.t()
  def cm_tree_spec() do
    new(32, 2, 256, :sha256)
  end

  # cairo poseidon cm tree spec
  @spec cairo_poseidon_cm_tree_spec() :: CommitmentTree.Spec.t()
  def cairo_poseidon_cm_tree_spec() do
    new(32, 2, 256, :poseidon)
  end

  # cairo poseidon resource tree spec for action
  @spec cairo_poseidon_resource_tree_spec() :: CommitmentTree.Spec.t()
  def cairo_poseidon_resource_tree_spec() do
    new(4, 2, 256, :poseidon)
  end

  @doc """
  I take an atom representation of the hash function used for the merkle
  tree and turn it into the appropriate function.
  """
  @spec hash_ref_to_hash(accumulator_hash()) :: function()
  def hash_ref_to_hash(:sha256) do
    fn {x, y} -> :crypto.hash(:sha256, x <> y) end
  end

  def hash_ref_to_hash(:poseidon) do
    fn {x, y} ->
      Cairo.poseidon(:binary.bin_to_list(x), :binary.bin_to_list(y))
      |> :binary.list_to_bin()
    end
  end

  defimpl Noun.Nounable, for: Spec do
    @impl true
    def to_noun(spec = %Spec{}) do
      [
        spec.depth,
        spec.splay,
        spec.key_size,
        Noun.Nounable.to_noun(spec.hash),
        spec.key_zero
        | spec.splay_suff_prod
      ]
    end
  end

  @spec from_noun(Noun.t()) :: :error | {:ok, t()} | :error
  def from_noun([depth, splay, key_size, hash, zero | prod]) do
    with {:ok, list} <- Noun.Nounable.List.from_noun(prod) do
      size = Noun.atom_binary_to_integer(key_size)

      {:ok,
       %__MODULE__{
         depth: Noun.atom_binary_to_integer(depth),
         splay: Noun.atom_binary_to_integer(splay),
         key_size: size,
         hash: hash |> Noun.atom_integer_to_binary() |> String.to_atom(),
         key_zero: <<Noun.atom_binary_to_integer(zero)::size(size)>>,
         splay_suff_prod: Enum.map(list, &Noun.atom_binary_to_integer/1)
       }}
    else
      _ -> :error
    end
  end
end
