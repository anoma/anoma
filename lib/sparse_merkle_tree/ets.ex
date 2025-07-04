defmodule SparseMerkleTree.ETS do
  @moduledoc """
  I implement a sparse Merkle tree using SHA-256 as an Erlang term.

  The implementation is still broadly the naive one, but using
  ETS-backed mutable storage.

  Because storage size is the main problem with the naive
  implementation, this does not help very much.
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

  @derive {Inspect, only: [:table]}
  typedstruct enforce: true do
    @typedoc """
    I am the struct representing a naive SHA-256 sparse merkle tree.

    I am mutable, operations update the underlying ETS table.
    """

    field(:table, :ets.tid())
  end

  # it's easier to define these in the module and use defdelegate to
  # build the impl, at the cost of some boilerplate here.
  defimpl SparseMerkleTree do
    defdelegate delete(tree), to: SparseMerkleTree.ETS
    defdelegate root(tree), to: SparseMerkleTree.ETS
    defdelegate insert(tree, leaf), to: SparseMerkleTree.ETS
    defdelegate insert_hash(tree, hash), to: SparseMerkleTree.ETS
    defdelegate present?(tree, leaf), to: SparseMerkleTree.ETS
    defdelegate hash_present?(tree, hash), to: SparseMerkleTree.ETS
    defdelegate sane?(tree), to: SparseMerkleTree.ETS
    defdelegate prove_present(tree, leaf), to: SparseMerkleTree.ETS
    defdelegate prove_absent(tree, leaf), to: SparseMerkleTree.ETS
  end

  @doc """
  I return a new sparse merkle tree.
  """
  @spec new() :: t()
  def new() do
    table = :ets.new(__MODULE__, [])
    %__MODULE__{table: table}
  end

  @doc """
  I delete a sparse merkle tree.
  """
  @spec delete(t()) :: :ok
  def delete(tree) do
    true = :ets.delete(tree.table)
    :ok
  end

  @doc """
  I return the root of a sparse merkle tree.
  """
  @spec root(t()) :: Hash.t()
  def root(tree) do
    case :ets.lookup(tree.table, <<>>) do
      [{<<>>, root}] -> root
      [] -> @default_hashes[0]
    end
  end

  @doc """
  I hash a leaf value and insert it into a sparse merkle tree, returning
  an updated tree.
  """
  @spec insert(t(), binary()) :: t()
  def insert(tree, leaf) do
    digest = hash(leaf)

    :ok = put_digest(tree.table, digest)

    tree
  end

  @doc """
  I insert a hash directly into a sparse merkle tree, returning an
  updated tree.
  """
  @spec insert_hash(t(), Hash.t()) :: t()
  def insert_hash(tree, hash) do
    :ok = put_digest(tree.table, hash)

    tree
  end

  @doc """
  I check whether a possible leaf's hash is present in a sparse merkle
  tree.
  """
  @spec present?(t(), binary()) :: bool()
  def present?(tree, leaf) do
    :ets.member(tree.table, hash(leaf))
  end

  @doc """
  I check whether a hash is present in a sparse merkle tree.
  """
  @spec hash_present?(t(), Hash.t()) :: bool()
  def hash_present?(tree, hash) do
    :ets.member(tree.table, hash)
  end

  @doc """
  I expensively check whether a struct representing a sparse merkle tree
  is sane, i.e., has the correct digests for its set of leaves.
  """
  @spec sane?(t()) :: bool()
  def sane?(_tree) do
    # todo: implement
    false
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
    # todo: prove more efficiently without copying
    temp_table = :ets.new(__MODULE__, [])
    :ets.insert(temp_table, :ets.tab2list(tree.table))

    {proof, <<>>} =
      for _ <- 256..0//-1, reduce: {[], hash(leaf)} do
        {hashes, bits} ->
          new_digest = fun.(temp_table, bits)

          :ets.insert(temp_table, {bits, new_digest})

          new_hashes =
            case bits do
              <<0::1, rest::bitstring>> ->
                r_key = <<1::1, rest::bitstring>>
                sibling_hash = get_digest(temp_table, r_key)
                [sibling_hash | hashes]

              <<1::1, rest::bitstring>> ->
                l_key = <<0::1, rest::bitstring>>
                sibling_hash = get_digest(temp_table, l_key)
                [sibling_hash | hashes]

              <<>> ->
                hashes
            end

          new_bits =
            case bits do
              <<_::1, new_bits::bitstring>> -> new_bits
              <<>> -> <<>>
            end

          {new_hashes, new_bits}
      end

    expected_root = root(tree)

    case :ets.lookup(temp_table, <<>>) do
      [{<<>>, ^expected_root}] ->
        :ets.delete(temp_table)
        {:ok, %Proof{hashes: Enum.reverse(proof)}}

      _ ->
        :ets.delete(temp_table)
        :error
    end
  end

  @spec put_digest(:ets.tid(), Hash.t()) :: :ok
  defp put_digest(table, hash) do
    for _ <- 256..0//-1, reduce: hash do
      bits ->
        :ets.insert(table, {bits, compute_digest(table, bits)})

        new_bits =
          case bits do
            <<_::1, new_bits::bitstring>> -> new_bits
            <<>> -> <<>>
          end

        new_bits
    end

    :ok
  end

  @spec compute_digest(:ets.tid(), Hash.t()) :: Hash.t()
  defp compute_digest(_table, _bits = <<_::256>>) do
    @present_hash
  end

  @spec compute_digest(:ets.tid(), bitstring()) :: Hash.t()
  defp compute_digest(table, bits) do
    l_key = <<(<<0::1>>), bits::bitstring>>
    r_key = <<(<<1::1>>), bits::bitstring>>

    l_digest = get_digest(table, l_key)
    r_digest = get_digest(table, r_key)

    hash(l_digest <> r_digest)
  end

  @spec compute_absence_digest(:ets.tid(), Hash.t()) :: Hash.t()
  defp compute_absence_digest(_digests, _bits = <<_::256>>) do
    @absent_hash
  end

  @spec compute_absence_digest(:ets.tid(), bitstring()) :: Hash.t()
  defp compute_absence_digest(table, bits) do
    compute_digest(table, bits)
  end

  @spec get_digest(:ets.tid(), bitstring()) :: Hash.t()
  defp get_digest(table, bits) do
    case :ets.lookup(table, bits) do
      [{^bits, result}] ->
        result

      [] ->
        @default_hashes[bit_size(bits)]
    end
  end
end
