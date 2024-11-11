defmodule SparseMerkleTree do
  @moduledoc """
  A sparse Merkle tree using SHA-256. Quite naive.
  """

  use Memoize
  use TypedStruct

  @type hash() :: <<_::256>>
  @type digest_map() :: %{bitstring() => hash()}

  @present_constant <<1>>
  @present_hash :crypto.hash(:sha256, @present_constant)
  @absent_constant <<>>
  @absent_hash :crypto.hash(:sha256, @absent_constant)

  typedstruct enforce: true do
    field(:leaves, MapSet.t(hash()), default: MapSet.new())
    field(:digests, digest_map(), default: %{})
    field(:root, hash())
  end

  @spec new() :: t()
  def new() do
    %__MODULE__{
      root: default_hash(0)
    }
  end

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

  @spec present?(t(), binary()) :: bool()
  def present?(tree, leaf) do
    MapSet.member?(tree.leaves, hash(leaf))
  end

  @spec prove_present(t(), binary()) :: {:ok, list(hash())} | :error
  def prove_present(tree, leaf) do
    prove(tree, leaf, &compute_digest/2)
  end

  @spec prove_absent(t(), binary()) :: {:ok, list(hash())} | :error
  def prove_absent(tree, leaf) do
    prove(tree, leaf, &compute_absence_digest/2)
  end

  @spec prove(t(), binary(), (digest_map(), bitstring() -> hash())) ::
          {:ok, list(hash())} | :error
  defp prove(tree, leaf, fun) do
    {proof, <<>>, _} =
      for _ <- 256..0//-1, reduce: {[], hash(leaf), %{}} do
        {hashes, bits, temp_digests} ->
          new_digest = fun.(temp_digests, bits)

          new_temp_digests = Map.put(temp_digests, bits, new_digest)

          new_hashes = [new_digest | hashes]

          new_bits =
            case bits do
              <<_::1, new_bits::bitstring>> -> new_bits
              <<>> -> <<>>
            end

          {new_hashes, new_bits, new_temp_digests}
      end

    expected_root = tree.root

    case proof do
      [^expected_root | _rest_of_proof] ->
        {:ok, proof}

      _ ->
        :error
    end
  end

  @spec put_digest(digest_map(), hash()) :: digest_map()
  defp put_digest(digests, hash) do
    {new_digests, <<>>} =
      for _ <- 256..0//-1, reduce: {digests, hash} do
        {digests, bits} ->
          new_digests = Map.put(digests, bits, compute_digest(digests, bits))

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

    hash_pair(l_digest, r_digest)
  end

  @spec compute_absence_digest(digest_map(), hash()) :: hash()
  defp compute_absence_digest(_digests, _bits = <<_::256>>) do
    @absent_hash
  end

  @spec compute_absence_digest(digest_map, bitstring()) :: hash()
  defp compute_absence_digest(digests, bits) do
    compute_digest(digests, bits)
  end

  @spec get_digest(digest_map(), bitstring()) :: hash()
  defp get_digest(digests, bits) do
    Map.get(digests, bits, default_hash(bit_size(bits)))
  end

  defp default_hash(_depth) do
    <<0::256>>
  end

  @spec hash_pair_specialized(hash(), hash()) :: hash()
  def hash_pair_specialized(l, r) when l != <<0::256>> and r != <<0::256>> do
    <<_::16, h::binary>> = hash_pair_sha256(l, r)
    <<1::16>> <> h
  end

  def hash_pair_specialized(<<0::256>>, <<0::256>>) do
    <<0::256>>
  end

  def hash_pair_specialized(l = <<1::1, _::255>>, r) do
    <<_::16, h::binary>> = hash_pair_sha256(l, r)
    <<1::16>> <> h
  end

  def hash_pair_specialized(l = <<0::16, _ :: 240>>, r) do
    <<_::16, h::binary>> = hash_pair_sha256(l, r)
    <<1::16>> <> h
  end

  def hash_pair_specialized(l, r = <<1::1, _::255>>) do
    <<_::16, h::binary>> = hash_pair_sha256(l, r)
    <<1::16>> <> h
  end

  def hash_pair_specialized(l, r = <<0::16, _ :: 240>>) do
    <<_::16, h::binary>> = hash_pair_sha256(l, r)
    <<1::16>> <> h
  end

  def hash_pair_specialized(l, <<0::256>>) do
    <<l::255, 0::1>>
  end

  def hash_pair_specialized(<<0::256>>, r) do
    <<r::255, 1::1>>
  end

  @spec hash(binary()) :: hash()
  defp hash(bytes) do
    hash_sha256(bytes)
  end

  @spec hash_pair(hash(), hash()) :: hash()
  defp hash_pair(l, r) do
    hash_pair_specialized(l, r)
  end

  @spec hash_pair_sha256(binary(), binary()) :: hash()
  defp hash_pair_sha256(l, r) do
    hash_sha256(l <> r)
  end

  @spec hash_sha256(binary()) :: hash()
  defp hash_sha256(bytes) do
    :crypto.hash(:sha256, bytes)
  end
end
