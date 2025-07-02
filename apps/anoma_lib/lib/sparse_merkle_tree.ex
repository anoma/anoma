defmodule SparseMerkleTree do
  @moduledoc """
  A Sparse Merkle Tree.
  """

  alias __MODULE__, as: Self

  use TypedStruct

  @type opts :: [depth: non_neg_integer()]

  @type hash :: <<_::256>>
  @type tree :: {hash() | nil, tree(), tree()} | {:leaf, hash() | nil}
  @type path :: bitstring
  @type proof_path :: [hash()]
  @type proof :: {:present, proof_path()} | {:absent, proof_path()}

  @empty_hash <<0::256>>
  @lpos 0
  @rpos 1

  typedstruct enforce: true do
    field(:depth, non_neg_integer())
    field(:root, tree(), default: {:leaf, nil})
  end

  defmodule CollisionError do
    @moduledoc false

    @enforce_keys [:data_hash, :collision_hash]
    defexception @enforce_keys

    @type t :: %__MODULE__{}

    @impl Exception
    def message(%__MODULE__{} = self) do
      Kernel.inspect(self)
    end
  end

  defmodule StaleTreeError do
    @moduledoc false

    defexception []

    @type t :: %__MODULE__{}

    @impl Exception
    def message(%__MODULE__{}) do
      "The provided tree has been modified but not yet re-hashed. Call " <>
        "#{__MODULE__}.rehash/1 first to avoid this error."
    end
  end

  @doc """
  Hash arbitrary binary data. Used for all tree hashes.
  """
  @spec hash(binary()) :: binary()
  def hash(data) when is_binary(data), do: :crypto.hash(:sha256, data)

  @doc """
  Returns an empty tree.

  The default tree depth is 256, but it can be overridden. Note that shallow
  trees are more likely to experience hash collisions.
  """
  @spec new(opts()) :: Self.t()
  def new(opts \\ []) do
    depth = Keyword.get(opts, :depth, 256)
    %Self{depth: depth}
  end

  @doc """
  Returns the cached root hash of the tree. Returns an error if the tree has
  been modified since it was last rehashed.
  """
  @spec root_hash(Self.t()) :: {:ok, hash()} | {:error, StaleTreeError.t()}
  def root_hash(%Self{} = self) do
    case cached_subtree_hash(self.root) do
      nil -> {:error, %StaleTreeError{}}
      hash -> {:ok, hash}
    end
  end

  @spec root_hash!(Self.t()) :: hash()
  def root_hash!(self), do: root_hash(self) |> unwrap_result_or_raise!()

  @doc """
  Put the given data hash in the tree.

  The return value will indicate whether the value was already present in the
  tree. If the tree depth is shallower than the number of bits in the hash
  function (256), it may be possible to experience a hash collision. In this
  case an error will be returned.

  Note that inserting a value will cause calculated hashes to be invalidated.
  To recalculate hashes, use the rehash/1 function.
  """
  @spec put(Self.t(), hash()) :: {:ok, Self.t()} | {:error, CollisionError.t()}
  def put(%Self{} = self, <<_::256>> = data_hash) do
    try do
      put_aux(self.root, path_for_data_hash(data_hash, self.depth), data_hash)
    rescue
      error -> {:error, error}
    else
      tree -> {:ok, %{self | root: tree}}
    end
  end

  @spec put!(Self.t(), hash()) :: Self.t()
  def put!(self, digest), do: put(self, digest) |> unwrap_result_or_raise!()

  @doc """
  Drops the given data hash from the tree.

  The return value will indicate whether the value was actually present in the
  tree. If the tree depth is shallower than the number of bits in the hash
  function (256), it may be possible to experience a hash collision. In this
  case an error will be returned.

  Note that inserting a value will cause calculated hashes to be invalidated.
  To recalculate hashes, use the rehash/1 function.
  """
  @spec drop(Self.t(), hash()) :: {:ok, Self.t()} | {:error, CollisionError.t()}
  def drop(%Self{} = self, <<_::256>> = data_hash) do
    try do
      drop_aux(self.root, path_for_data_hash(data_hash, self.depth), data_hash)
    rescue
      error -> {:error, error}
    else
      tree -> {:ok, %{self | root: tree}}
    end
  end
  
  @spec drop!(Self.t(), hash()) :: Self.t() | {:error, CollisionError.t()}
  def drop!(self, digest), do: drop(self, digest) |> unwrap_result_or_raise!()

  @doc """
  Recalculates any missing hashes throughout the tree.
  """
  @spec rehash(Self.t()) :: Self.t()
  def rehash(%Self{} = self) do
    {root, _root_hash} = rehash_subtree(self.root)
    %{self | root: root}
  end

  @doc """
  Returns a proof of the presence or absence of the given hash in the tree.
  The proof consists of a list representing the path between the tree root
  and the leaf node for the data hash. Each path element is the hash of the
  sibling branch at that node.
  """
  @spec proof(Self.t(), hash()) :: {:ok, proof()} | {:error, StaleTreeError.t()} | {:error, CollisionError.t()}
  def proof(%Self{} = self, <<_::256>> = data_hash) do
    path = path_for_data_hash(data_hash, self.depth)
    try do
      subtree_proof(self.root, path, data_hash)
    rescue
      error -> {:error, error}
    else
      {presence, proof} -> {:ok, {presence, proof}}
    end
  end

  @spec proof!(Self.t(), hash()) :: proof()
  def proof!(self, digest), do: proof(self, digest) |> unwrap_result_or_raise!()

  @doc """
  Checks the validity of a proof as returned by the proof/2 function. This
  works both for presence proofs and absence proofs.
  """
  @spec valid_proof?(hash(), proof(), hash()) :: boolean()
  def valid_proof?(<<_::256>> = data_hash, {presence, proof_path}, <<_::256>> = root_hash)
  when presence in [:present, :absent] and is_list(proof_path) do
    max_tree_depth = Enum.count(proof_path)
    # For absence proofs we actually prove the membership of the empty hash
    initial_hash =
      case presence do
        :present -> data_hash
        :absent -> @empty_hash
      end
    # Reverse the node path since we start from the leaf
    rev_path = reverse_bits(path_for_data_hash(data_hash, max_tree_depth), <<>>)
    # Reverse the proof path since we start from the leaf
    rev_proof_path = Enum.reverse(proof_path)
    # Ensure that the implied Merkle tree root is the correct one
    root_hash == valid_proof_aux(initial_hash, rev_path, rev_proof_path)
  end

  # Reversing an empty bitstring is trivial
  def reverse_bits(<<>>, acc), do: acc
  # Otherwise move current bit to the beginning of accumulator
  def reverse_bits(<<hd::1, tl::bitstring>>, acc) do
    reverse_bits(tl, <<hd::1, acc::bitstring>>)
  end

  # Empty paths imply the trivial root hash
  def valid_proof_aux(acc_hash, <<>>, []), do: acc_hash
  # Combine a left node with a sibling from the right
  def valid_proof_aux(acc_hash, <<@lpos::1, path_tl::bitstring>>, [sibling_hash | proof_path_tl]) do
    valid_proof_aux(hash(acc_hash <> sibling_hash), path_tl, proof_path_tl)
  end
  # Combine a right node with a sibling from the left
  def valid_proof_aux(acc_hash, <<@rpos::1, path_tl::bitstring>>, [sibling_hash | proof_path_tl]) do
    valid_proof_aux(hash(sibling_hash <> acc_hash), path_tl, proof_path_tl)
  end

  @spec path_for_data_hash(binary(), non_neg_integer()) :: path()
  # Turn the data hash into the path where it will be stored
  defp path_for_data_hash(data_hash, tree_depth) do
    <<path::bitstring-size(tree_depth), _::bitstring>> = data_hash
    path
  end

  @spec put_aux(tree(), path(), hash()) :: tree()
  # Inserting at empty leaf fills the leaf
  defp put_aux({:leaf, nil}, <<>>, data_hash), do: {:leaf, data_hash}
  # Inserting at identical leaf does nothing
  defp put_aux(tree = {:leaf, digest}, <<>>, digest), do: tree
  # Inserting at unmatched leaf raises exception
  defp put_aux({:leaf, leaf_hash}, <<>>, digest) when leaf_hash != digest do
    raise %CollisionError{data_hash: digest, collision_hash: leaf_hash}
  end
  # Make a left branch to put the data into
  defp put_aux(empty = {:leaf, nil}, <<@lpos::1, path_tl::bitstring>>, data_hash) do
    {nil, put_aux(empty, path_tl, data_hash), empty}
  end
  # Make a right branch to put the data into
  defp put_aux(empty = {:leaf, nil}, <<@rpos::1, path_tl::bitstring>>, data_hash) do
    {nil, empty, put_aux(empty, path_tl, data_hash)}
  end
  # Insert the data into the left branch
  defp put_aux({_, left, right}, <<@lpos::1, path_tl::bitstring>>, digest) do
    {nil, put_aux(left, path_tl, digest), right}
  end
  # Insert the data into the right branch
  defp put_aux({_, left, right}, <<@rpos::1, path_tl::bitstring>>, digest) do
    {nil, left, put_aux(right, path_tl, digest)}
  end

  # Bring a branch with two empty children into canonical form
  defp collapse_if_empty({nil, {:leaf, nil}, {:leaf, :nil}}), do: {:leaf, nil}
  # Otherwise leave the branch as is
  defp collapse_if_empty(tree), do: tree

  @spec drop_aux(tree(), path(), hash()) :: tree()
  # Dropping from an empty tree leaves it unchanged
  defp drop_aux(empty = {:leaf, nil}, _path, _data_hash), do: empty
  # Drop a digest from the left branch
  defp drop_aux({_, left, right}, <<@lpos::1, path_tl::bitstring>>, digest) do
    collapse_if_empty({nil, drop_aux(left, path_tl, digest), right})
  end
  # Drop a digest from the right branch
  defp drop_aux({_, left, right}, <<@rpos::1, path_tl::bitstring>>, digest) do
    collapse_if_empty({nil, left, drop_aux(right, path_tl, digest)})
  end
  # Tree becomes empty if its only digest is removed
  defp drop_aux({:leaf, data_hash}, <<>>, data_hash), do: {:leaf, nil}
  # Error out if a hash conflict is detected
  defp drop_aux({:leaf, leaf_hash}, <<>>, digest) when leaf_hash != digest do
    raise %CollisionError{data_hash: digest, collision_hash: leaf_hash}
  end

  # Refreshes the cached hashes in the given subtree.
  @spec rehash_subtree(tree()) :: {tree(), hash()}
  # Fill the branch hash when there is none     
  defp rehash_subtree({nil, left, right}) do
    {new_left, left_hash} = rehash_subtree(left)
    {new_right, right_hash} = rehash_subtree(right)
    new_hash = hash_pair(left_hash, right_hash)
    new_tree = {new_hash, new_left, new_right}
    {new_tree, new_hash}
  end
  # Extract the branch hash when there is one
  defp rehash_subtree(tree = {hash, _left, _right}), do: {tree, hash}
  # Return the empty hash for a blank leaf
  defp rehash_subtree(empty = {:leaf, nil}), do: {empty, @empty_hash}
  # Return the hash stored in the leaf
  defp rehash_subtree(leaf = {:leaf, hash}), do: {leaf, hash}

  @spec hash_pair(hash(), hash()) :: hash()
  # Compute the hash for a Merkle tree branch
  defp hash_pair(left, right) do
    hash(left <> right)
  end

  @spec cached_subtree_hash(tree()) :: hash() | nil
  # The hash for a blank leaf is the empty hash
  defp cached_subtree_hash({:leaf, nil}), do: @empty_hash
  # Extract the branch hash from the branch
  defp cached_subtree_hash({branch_hash, _left, _right}), do: branch_hash
  # Extract the hash from a filled leaf
  defp cached_subtree_hash({:leaf, data_hash}), do: data_hash

  @spec cached_subtree_hash!(tree()) :: hash()
  # Extract hash from the given tree and throw exception if there's none
  defp cached_subtree_hash!(tree) do
    case cached_subtree_hash(tree) do
      nil -> raise %StaleTreeError{}
      hash -> hash
    end
  end

  @spec subtree_proof(tree(), path(), hash()) :: proof() | {:collision, hash()}
  # An empty tree always generates absence proofs
  defp subtree_proof({:leaf, nil}, _path, _data_hash), do: {:absent, []}
  # Prove the presence of the digest in the left subtree
  defp subtree_proof({_, left, right}, <<@lpos::1, path_tl::bitstring>>, data_hash) do
    # Get the sibling hash
    proof_path_element = cached_subtree_hash!(right)
    # Prove the presence of the digest in the left subtree
    {presence, proof_subpath} = subtree_proof(left, path_tl, data_hash)
    # Prefix the left subtree proof
    {presence, [proof_path_element | proof_subpath]}
  end
  # Prove the presence of the digest in the right subtree
  defp subtree_proof({_, left, right}, <<@rpos::1, path_tl::bitstring>>, data_hash) do
    # Get the sibling hash
    proof_path_element = cached_subtree_hash!(left)
    # Prove the presence of the digest in the right subtree
    {presence, proof_subpath} = subtree_proof(right, path_tl, data_hash)
    # Prefix the right subtree proof
    {presence, [proof_path_element | proof_subpath]}
  end
  # If the tree is the queried digest, then it's trivially present
  defp subtree_proof({:leaf, data_hash}, <<>>, data_hash), do: {:present, []}
  # Encountered a hash conflict, so error out
  defp subtree_proof({:leaf, leaf_hash}, <<>>, digest)
       when leaf_hash != digest do
    raise %CollisionError{data_hash: digest, collision_hash: leaf_hash}
  end

  @spec unwrap_result_or_raise!({:ok, out} | {:error, Exception.t()}) :: out when out: any()
  # Convert ok result into the result itself
  defp unwrap_result_or_raise!({:ok, out}), do: out
  # But turn an error result into an exception
  defp unwrap_result_or_raise!({:error, e}) when is_exception(e), do: raise(e)
end
