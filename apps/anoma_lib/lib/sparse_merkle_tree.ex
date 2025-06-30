defmodule SparseMerkleTree do
  @moduledoc """
  A sparse Merkle tree using SHA-256. Quite naive.
  """

  use Memoize
  use TypedStruct

  @type hash() :: <<_::256>>
  @type digest_map() :: %{bitstring() => hash()}

  # A special sentinel value representing the absence of a node
  @empty_hash <<0::256>>

  typedstruct enforce: true do
    # The depth of the Merkle tree
    field(:depth, non_neg_integer())
    # A map from node paths (represented as bitstrings) to their digest
    field(:digests, digest_map(), default: %{})
  end

  @spec new() :: t()
  def new(opts \\ []) do
    # Assume a default depth of 256
    depth = Keyword.get(opts, :depth, 256)
    %__MODULE__{ depth: depth }
  end

  # Insert the given leaf into the tree
  @spec insert(t(), binary()) :: t()
  def insert(tree, leaf) do
    # Only the prefix of the digest is used as the path
    <<path::bitstring-size(tree.depth), _::bitstring>> = digest = hash(leaf)
    # Ensure that the stored digest matches the query
    if Map.get(tree.digests, path, digest) != digest, do: raise "Detected hash conflict"
    # Store the leaf digest
    digests = Map.put(tree.digests, path, digest)
    # Update the Merkle tree digests
    new_digests = update_digests(digests, path)
    %__MODULE__{digests: new_digests, depth: tree.depth}
  end

  # Remove the given leaf from the tree
  def remove(tree, leaf) do
    # Only the prefix of the digest is used as the path
    <<path::bitstring-size(tree.depth), _::bitstring>> = digest = hash(leaf)
    # Ensure that the stored digest matches the query
    if Map.get(tree.digests, path, digest) != digest, do: raise "Detected hash conflict"
    # Remove the leaf
    digests = Map.delete(tree.digests, path)
    # Update the ancestor digests
    new_digests = update_digests(digests, path)
    %__MODULE__{digests: new_digests, depth: tree.depth}
  end

  # Compute the root hash of the given tree
  def root(tree) do
    # The root digest is stored at the empty path
    {Map.get(tree.digests, <<>>, @empty_hash), tree.depth}
  end

  # Check if the given leaf is present in the tree
  @spec present?(t(), binary()) :: bool()
  def present?(tree, leaf) do
    # Only the prefix of the digest is used as the path
    <<path::bitstring-size(tree.depth), _::bitstring>> = digest = hash(leaf)
    # Ensure that the stored digest matches the query
    Map.get(tree.digests, path) == digest
  end

  # Prove that the given node is present or absent from the tree
  defp prove_aux(_tree, <<>>, auth_path) do
    # Reverse the path so that earlier elements represent lower hashes
    Enum.reverse(auth_path)
  end

  defp prove_aux(tree, path, proof) do
    <<path_hd::1, path_tl::bitstring>> = path
    if Map.has_key?(tree.digests, path_tl) do
      # Obtain the sibling hash in order to prove this node is in parent
      sibling_path = <<(1-path_hd)::1, path_tl::bitstring>>
      sibling_digest = Map.get(tree.digests, sibling_path, @empty_hash)
      # Prove that the parent node is also in the tree
      prove_aux(tree, path_tl, [sibling_digest | proof])
    else
      # If the parent node not in tree, then sibling is not required
      prove_aux(tree, path_tl, proof)
    end
  end

  # Prove that the given leaf is present or absent from the tree
  @spec prove(t(), binary()) :: {bool(), list(hash())}
  def prove(tree, leaf) do
    # Only the prefix of the digest is used as the path
    <<path::bitstring-size(tree.depth), _::bitstring>> = digest = hash(leaf)
    # Are we proving that the leaf is present or absent?
    stored_digest = Map.get(tree.digests, path)
    present = stored_digest != nil
    # Ensure that the stored digest matches the query
    if present && stored_digest != digest, do: raise "Detected hash conflict"
    # Produce a Merkle proof for the given path
    proof = prove_aux(tree, path, [])
    # Indicate the presence of this leaf and a proof for it
    {present, proof}
  end

  # Update the digests of the given node's ancestors
  defp update_digests(digests, <<>>) do
    digests
  end

  defp update_digests(digests, <<_path_hd::1, path_tl::bitstring>>) do
    new_digests = case compute_digest(digests, path_tl) do
      # Remove the parent entry because no children exist
      nil -> Map.delete(digests, path_tl)
      # Store the digest for the current path
      digest ->Map.put(digests, path_tl, digest)
    end
    # Store the digests for ancestors
    update_digests(new_digests, path_tl)
  end

  def verify_aux(root, node, <<>>, []) do
    # For a zero length path, the root must equal the node
    root == node
  end

  def verify_aux(root, node, <<path_hd::1, path_tl::bitstring>>, proof) do
    # Use the proof's head to construct the next parent
    [proof_hd | proof_tl] = proof
    parent_node = if path_hd == 1 do
      # If the node is in the right position, then hash accordingly
      hash_pair(proof_hd, node)
    else
      # If the node is in the left position, then hash accordingly
      hash_pair(node, proof_hd)
    end
    # Verify that the parent node is in the root hash
    verify_aux(root, parent_node, path_tl, proof_tl)
  end
  
  def verify({root, depth}, leaf, {present, proof}) do
    proof_length = Enum.count(proof)
    # Ensure that we are checking the membership of a leaf. Proofs generated by
    # prove always pass these checks.
    if proof_length > depth, do: raise "Proof length must not exceed tree depth"
    if present && proof_length < depth, do: raise "Presence proof length must equal tree depth"
    # Obtain the path suffix used in the proof
    <<_::bitstring-size(depth-proof_length), path::bitstring-size(proof_length), _::bitstring>> = hash(leaf)
    # If it's an absence proof, then actually prove that empty hash is at path
    leaf_digest = if present, do: hash(leaf), else: @empty_hash
    # Finally actually prove that leaf is in root through the given path
    verify_aux(root, leaf_digest, path, proof)
  end

  # Compute the digest for a parent node
  @spec compute_digest(t(), bitstring()) :: hash()
  defp compute_digest(digests, path) do
    # Get the digest for the left child
    left_path = <<0::1, path::bitstring>>
    left_digest = Map.get(digests, left_path)
    # Get the digest for the right child
    right_path = <<1::1, path::bitstring>>
    right_digest = Map.get(digests, right_path)
    # Combine the child digests
    hash_pair(left_digest, right_digest)
  end

  # Compute the digest for a leaf node
  @spec hash(binary()) :: hash()
  def hash(bytes) do
    :crypto.hash(:sha256, bytes)
  end

  # Compute digest of parent node from two children
  @spec hash_pair(hash(), hash()) :: hash()
  # No children means no hash
  defp hash_pair(nil, nil), do: nil
  # An empty right child is treated as the empty hash
  defp hash_pair(left, nil), do: :crypto.hash(:sha256, left <> @empty_hash)
  # An empty left child is treated as the empty hash
  defp hash_pair(nil, right), do: :crypto.hash(:sha256, @empty_hash <> right)
  # Otherwise hash the pair as normal
  defp hash_pair(left, right), do: :crypto.hash(:sha256, left <> right)
end
