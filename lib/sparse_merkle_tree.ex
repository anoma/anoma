defprotocol SparseMerkleTree do
  @moduledoc """
  I define the generic sparse merkle tree protocol.
  """

  # new takes no arguments, and cannot be a protocol member.
  def delete(tree)
  def root(tree)
  def insert(tree, leaf)
  def insert_hash(tree, hash)
  def present?(tree, leaf)
  def hash_present?(tree, hash)
  def sane?(tree)
  def prove_present(tree, leaf)
  def prove_absent(tree, leaf)
end

defmodule SparseMerkleTree.Hash do
  @moduledoc """
  I define the hash function for all implementations in one place,
  though references to its length are more widespread.
  """

  @type t() :: <<_::256>>

  @spec hash(binary()) :: t()
  def hash(bytes) do
    :crypto.hash(:sha256, bytes)
  end
end

defmodule SparseMerkleTree.Proof do
  @moduledoc """
  I define the common proof and verification interface for all sparse
  merkle tree implementations.
  """

  alias SparseMerkleTree.Hash
  import SparseMerkleTree.Hash
  use TypedStruct

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

  @derive {Inspect, only: []}
  typedstruct enforce: true do
    field(:hashes, list(Hash.t()))
  end

  def present_constant, do: @present_constant
  def present_hash, do: @present_hash
  def absent_constant, do: @absent_constant
  def absent_hash, do: @absent_hash
  def default_hashes, do: @default_hashes

  @doc """
  I verify that a proof shows a leaf is present in a tree with the given
  root.
  """
  @spec verify_present(t(), Hash.t(), binary()) :: bool()
  def verify_present(proof, root, leaf) do
    verify(proof, root, leaf, @present_hash)
  end

  @doc """
  I verify that a proof shows a leaf is absent in a tree with the given
  root.
  """
  @spec verify_absent(t(), Hash.t(), binary()) :: bool()
  def verify_absent(proof, root, leaf) do
    verify(proof, root, leaf, @absent_hash)
  end

  @spec verify(t(), Hash.t(), binary(), Hash.t()) :: bool()
  defp verify(proof, root, leaf, leaf_value) do
    proof_hashes = proof.hashes

    {<<>>, computed_root} =
      for sibling_hash <- proof_hashes,
          reduce: {hash(leaf), leaf_value} do
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
end
