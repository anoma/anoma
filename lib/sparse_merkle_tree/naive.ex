# As a hack to let us use the hash helper during the compilation of the
# main module, predefine it in its own module first.
defmodule SparseMerkleTree.Naive.Hash do
  @spec hash(binary()) :: <<_::256>>
  def hash(bytes) do
    :crypto.hash(:sha256, bytes)
  end
end

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

  import SparseMerkleTree.Naive.Hash

  @type hash() :: <<_::256>>
  @type digest_map() :: %{bitstring() => hash()}
  @type proof() :: list(hash())

  # precompute all our default hashes at compile time.
  @present_constant "t"
  @present_hash hash(@present_constant)
  @absent_constant "f"
  @absent_hash hash(@absent_constant)

  @default_hashes (for depth <- 255..0//-1,
                       reduce: %{256 => @absent_hash} do
                     acc ->
                       hash_below = acc[depth + 1]

                       Map.put(
                         acc,
                         depth,
                         hash(hash_below <> hash_below)
                       )
                   end)

  @derive {Inspect, only: [:root]}
  typedstruct enforce: true do
    @typedoc """
    I am the struct representing a naive SHA-256 sparse merkle tree.

    I am immutable, all operations return a new value.
    """

    field(:leaves, MapSet.t(hash()), default: MapSet.new())
    field(:digests, digest_map(), default: %{})
    field(:root, hash(), default: @default_hashes[0])
  end

  @doc """
  I return a new sparse merkle tree.
  """
  @spec new() :: t()
  def new() do
    %__MODULE__{}
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
  @spec insert_direct(t(), hash()) :: t()
  def insert_direct(tree, hash) do
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
  I expensively check whether a struct representing a sparse merkle tree
  is sane, i.e., has the correct digests for its set of leaves.
  """
  @spec sane?(t()) :: bool()
  def sane?(tree) do
    tree ==
      for leaf_hash <- tree.leaves, reduce: new() do
        tree -> insert_direct(tree, leaf_hash)
      end
  end

  @doc """
  I try to prove a leaf's hash is present in a sparse merkle tree,
  returning `{:ok, proof}` if it is, or else `:error`.
  """
  @spec prove_present(t(), binary()) :: {:ok, proof()} | :error
  def prove_present(tree, leaf) do
    prove(tree, leaf, &compute_digest/2)
  end

  @doc """
  I try to prove a leaf's hash is absent in a sparse merkle tree,
  returning `{:ok, proof}` if it is, or else `:error`.
  """
  @spec prove_absent(t(), binary()) :: {:ok, proof()} | :error
  def prove_absent(tree, leaf) do
    prove(tree, leaf, &compute_absence_digest/2)
  end

  @doc """
  I verify that a proof shows a leaf is present in a tree with the given
  root.
  """
  @spec verify_present(proof(), hash(), binary()) :: bool()
  def verify_present(proof, root, leaf) do
    verify(proof, root, leaf, @present_hash)
  end

  @doc """
  I verify that a proof shows a leaf is absent in a tree with the given
  root.
  """
  @spec verify_absent(proof(), hash(), binary()) :: bool()
  def verify_absent(proof, root, leaf) do
    verify(proof, root, leaf, @absent_hash)
  end

  @spec prove(t(), binary(), (digest_map(), bitstring() -> hash())) ::
          {:ok, proof()} | :error
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
      ^expected_root -> {:ok, Enum.reverse(proof)}
      _ -> :error
    end
  end

  @spec verify(proof(), hash(), binary(), hash()) :: bool()
  defp verify(proof, root, leaf, leaf_value) do
    {<<>>, computed_root} =
      for sibling_hash <- proof, reduce: {hash(leaf), leaf_value} do
        {bits, current_hash} ->
          case bits do
            <<0::1, rest::bitstring>> ->
              {rest, hash(current_hash <> sibling_hash)}

            <<1::1, rest::bitstring>> ->
              {rest, hash(sibling_hash <> current_hash)}

            <<>> ->
              raise("proof too long")
          end
      end

    root == computed_root
  end

  @spec put_digest(digest_map(), hash()) :: digest_map()
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

  @spec compute_digest(digest_map(), hash()) :: hash()
  defp compute_digest(_digests, _bits = <<_::256>>) do
    @present_hash
  end

  @spec compute_digest(digest_map(), bitstring()) :: hash()
  defp compute_digest(digests, bits) do
    l_key = <<(<<0::1>>), bits::bitstring>>
    r_key = <<(<<1::1>>), bits::bitstring>>

    l_digest = get_digest(digests, l_key)
    r_digest = get_digest(digests, r_key)

    hash(l_digest <> r_digest)
  end

  @spec compute_absence_digest(digest_map(), hash()) :: hash()
  defp compute_absence_digest(_digests, _bits = <<_::256>>) do
    @absent_hash
  end

  @spec compute_absence_digest(digest_map(), bitstring()) :: hash()
  defp compute_absence_digest(digests, bits) do
    compute_digest(digests, bits)
  end

  @spec get_digest(digest_map(), bitstring()) :: hash()
  defp get_digest(digests, bits) do
    Map.get(digests, bits, default_hash(bit_size(bits)))
  end

  @spec default_hash(0..256) :: hash()
  defp default_hash(depth) do
    @default_hashes[depth]
  end
end
