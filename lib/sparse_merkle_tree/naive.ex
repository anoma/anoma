defmodule SparseMerkleTree.Naive do
  @moduledoc """
  I implement a sparse Merkle tree using SHA-256 as an Erlang term.

  The implementation is quite naive (not maximally naive, which would
  mean storing a depth-256 binary tree, but pretty naive for things
  which actually fit in memory).

  Other implementations should have the same behavior, but possibly
  improved performance, and possibly use mutable storage.
  """

  use TypedStruct

  alias SparseMerkleTree.Hash
  import SparseMerkleTree.Hash, only: [hash: 1]
  alias SparseMerkleTree.Proof

  @type digest_map() :: %{bitstring() => Hash.t()}

  # precompute all our default hashes at compile time.
  @present_hash Proof.present_hash()
  @absent_hash Proof.absent_hash()

  @default_hashes Proof.default_hashes()

  @derive {Inspect, only: [:root]}
  typedstruct enforce: true do
    @typedoc """
    I am the struct representing a naive SHA-256 sparse merkle tree.

    I am immutable, all operations return a new value.
    """

    field(:leaves, MapSet.t(Hash.t()), default: MapSet.new())
    field(:digests, digest_map(), default: %{})
    field(:root, Hash.t(), default: @default_hashes[0])
  end

  # it's easier to define these in the module and use defdelegate to
  # build the impl, at the cost of some boilerplate here.
  defimpl SparseMerkleTree do
    defdelegate delete(tree), to: SparseMerkleTree.Naive
    defdelegate root(tree), to: SparseMerkleTree.Naive
    defdelegate insert(tree, leaf), to: SparseMerkleTree.Naive
    defdelegate insert_hash(tree, hash), to: SparseMerkleTree.Naive
    defdelegate present?(tree, leaf), to: SparseMerkleTree.Naive
    defdelegate hash_present?(tree, hash), to: SparseMerkleTree.Naive
    defdelegate sane?(tree), to: SparseMerkleTree.Naive
    defdelegate prove_present(tree, leaf), to: SparseMerkleTree.Naive
    defdelegate prove_absent(tree, leaf), to: SparseMerkleTree.Naive
  end

  @doc """
  I return a new sparse merkle tree.
  """
  @spec new() :: t()
  def new() do
    %__MODULE__{}
  end

  @doc """
  I delete a sparse merkle tree (a no-op here, because the trees are
  immutable terms in this implementation).
  """
  @spec delete(t()) :: :ok
  def delete(_tree) do
    :ok
  end

  @doc """
  I return the root of a sparse merkle tree.
  """
  @spec root(t()) :: Hash.t()
  def root(tree) do
    tree.root
  end

  @doc """
  I hash a leaf value and insert it into a sparse merkle tree, returning
  an updated tree.
  """
  @spec insert(t(), binary()) :: t()
  def insert(tree, leaf) do
    digest = hash(leaf)

    new_leaves = MapSet.put(tree.leaves, digest)
    new_digests = put_digest(tree.digests, digest)
    new_root = Map.get(new_digests, <<>>)

    %__MODULE__{
      leaves: new_leaves,
      digests: new_digests,
      root: new_root
    }
  end

  @doc """
  I insert a hash directly into a sparse merkle tree, returning an
  updated tree.
  """
  @spec insert_hash(t(), Hash.t()) :: t()
  def insert_hash(tree, hash) do
    new_leaves = MapSet.put(tree.leaves, hash)
    new_digests = put_digest(tree.digests, hash)
    new_root = Map.get(new_digests, <<>>)

    %__MODULE__{
      leaves: new_leaves,
      digests: new_digests,
      root: new_root
    }
  end

  @doc """
  I check whether a possible leaf's hash is present in a sparse merkle
  tree.
  """
  @spec present?(t(), binary()) :: bool()
  def present?(tree, leaf) do
    MapSet.member?(tree.leaves, hash(leaf))
  end

  @doc """
  I check whether a hash is present in a sparse merkle tree.
  """
  @spec hash_present?(t(), Hash.t()) :: bool()
  def hash_present?(tree, hash) do
    MapSet.member?(tree.leaves, hash)
  end

  @doc """
  I expensively check whether a struct representing a sparse merkle tree
  is sane, i.e., has the correct digests for its set of leaves.
  """
  @spec sane?(t()) :: bool()
  def sane?(tree) do
    tree ==
      for leaf_hash <- tree.leaves, reduce: new() do
        tree -> insert_hash(tree, leaf_hash)
      end
  end

  @doc """
  I try to prove a leaf's hash is present in a sparse merkle tree,
  returning `{:ok, proof}` if it is, or else `:error`.
  """
  @spec prove_present(t(), binary()) :: {:ok, Proof.t()} | :error
  def prove_present(tree, leaf) do
    prove(tree, leaf, &compute_digest/2)
  end

  @doc """
  I try to prove a leaf's hash is absent in a sparse merkle tree,
  returning `{:ok, proof}` if it is, or else `:error`.
  """
  @spec prove_absent(t(), binary()) :: {:ok, Proof.t()} | :error
  def prove_absent(tree, leaf) do
    prove(tree, leaf, &compute_absence_digest/2)
  end

  @spec prove(t(), binary(), (digest_map(), bitstring() -> Hash.t())) ::
          {:ok, Proof.t()} | :error
  defp prove(tree, leaf, fun) do
    {proof, <<>>, final_digests} =
      for _ <- 256..0//-1, reduce: {[], hash(leaf), tree.digests} do
        {hashes, bits, temp_digests} ->
          new_digest = fun.(temp_digests, bits)

          new_temp_digests = Map.put(temp_digests, bits, new_digest)

          new_hashes =
            case bits do
              <<0::1, rest::bitstring>> ->
                r_key = <<1::1, rest::bitstring>>
                sibling_hash = get_digest(temp_digests, r_key)
                [sibling_hash | hashes]

              <<1::1, rest::bitstring>> ->
                l_key = <<0::1, rest::bitstring>>
                sibling_hash = get_digest(temp_digests, l_key)
                [sibling_hash | hashes]

              <<>> ->
                hashes
            end

          new_bits =
            case bits do
              <<_::1, new_bits::bitstring>> -> new_bits
              <<>> -> <<>>
            end

          {new_hashes, new_bits, new_temp_digests}
      end

    expected_root = tree.root

    case Map.get(final_digests, <<>>) do
      ^expected_root -> {:ok, %Proof{hashes: Enum.reverse(proof)}}
      _ -> :error
    end
  end

  @spec put_digest(digest_map(), Hash.t()) :: digest_map()
  defp put_digest(digests, hash) do
    {new_digests, <<>>} =
      for _ <- 256..0//-1, reduce: {digests, hash} do
        {digests, bits} ->
          new_digests =
            Map.put(digests, bits, compute_digest(digests, bits))

          new_bits =
            case bits do
              <<_::1, new_bits::bitstring>> -> new_bits
              <<>> -> <<>>
            end

          {new_digests, new_bits}
      end

    new_digests
  end

  @spec compute_digest(digest_map(), Hash.t()) :: Hash.t()
  defp compute_digest(_digests, _bits = <<_::256>>) do
    @present_hash
  end

  @spec compute_digest(digest_map(), bitstring()) :: Hash.t()
  defp compute_digest(digests, bits) do
    l_key = <<(<<0::1>>), bits::bitstring>>
    r_key = <<(<<1::1>>), bits::bitstring>>

    l_digest = get_digest(digests, l_key)
    r_digest = get_digest(digests, r_key)

    hash(l_digest <> r_digest)
  end

  @spec compute_absence_digest(digest_map(), Hash.t()) :: Hash.t()
  defp compute_absence_digest(_digests, _bits = <<_::256>>) do
    @absent_hash
  end

  @spec compute_absence_digest(digest_map(), bitstring()) :: Hash.t()
  defp compute_absence_digest(digests, bits) do
    compute_digest(digests, bits)
  end

  @spec get_digest(digest_map(), bitstring()) :: Hash.t()
  defp get_digest(digests, bits) do
    Map.get(digests, bits, @default_hashes[bit_size(bits)])
  end
end
