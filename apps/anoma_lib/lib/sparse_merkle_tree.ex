defmodule SparseMerkleTree do
  @moduledoc """
  A Sparse Merkle Tree.
  """

  alias __MODULE__, as: Self

  use TypedStruct

  @type opts :: [depth: non_neg_integer()]

  @type hash :: <<_::256>>
  @type branch_hash :: :no_hash | hash()
  @type tree :: :empty | {branch_hash(), tree(), tree()} | {:leaf, hash()}
  @type path :: [:left | :right]
  @type proof_path :: [hash()]
  @type proof :: {:present, proof_path()} | {:absent, proof_path()}

  @empty_hash <<0::256>>

  typedstruct enforce: true do
    field(:depth, non_neg_integer())
    field(:root, tree(), default: :empty)
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
  def hash(data) when is_binary(data) do
    :crypto.hash(:sha256, data)
  end

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
      {:ok, hash} -> {:ok, hash}
      :none -> {:error, %StaleTreeError{}}
    end
  end

  @spec root_hash!(Self.t()) :: hash()
  def root_hash!(self) do
    root_hash(self)
    |> unwrap_result_or_raise!()
  end

  @doc """
  Put the given data hash in the tree.

  The return value will indicate whether the value was already present in the
  tree. If the tree depth is shallower than the number of bits in the hash
  function (256), it may be possible to experience a hash collision. In this
  case an error will be returned.

  Note that inserting a value will cause calculated hashes to be invalidated.
  To recalculate hashes, use the rehash/1 function.
  """
  @spec put(Self.t(), hash()) ::
          {:ok, {leaf_status, Self.t()}} | {:error, CollisionError.t()}
        when leaf_status: :inserted | :present
  def put(%Self{} = self, <<_::256>> = data_hash) do
    path = path_for_data_hash(data_hash, self.depth)

    case put_at_path(self.root, path, data_hash) do
      {:inserted, tree} ->
        {:ok, {:inserted, %{self | root: tree}}}

      :present ->
        {:ok, {:present, self}}

      {:collision, collision_hash} ->
        {:error,
         %CollisionError{
           data_hash: data_hash,
           collision_hash: collision_hash
         }}
    end
  end

  @spec put!(Self.t(), hash()) :: {leaf_status, Self.t()}
        when leaf_status: :inserted | :present
  def put!(self, data_hash) do
    put(self, data_hash)
    |> unwrap_result_or_raise!()
  end

  @doc """
  Drops the given data hash from the tree.

  The return value will indicate whether the value was actually present in the
  tree. If the tree depth is shallower than the number of bits in the hash
  function (256), it may be possible to experience a hash collision. In this
  case an error will be returned.

  Note that inserting a value will cause calculated hashes to be invalidated.
  To recalculate hashes, use the rehash/1 function.
  """
  @spec drop(Self.t(), hash()) ::
          {:ok, {leaf_status, Self.t()}} | {:error, CollisionError.t()}
        when leaf_status: :dropped | :absent
  def drop(%Self{} = self, <<_::256>> = data_hash) do
    path = path_for_data_hash(data_hash, self.depth)

    case drop_at_path(self.root, path, data_hash) do
      {:dropped, tree} ->
        {:ok, {:dropped, %{self | root: tree}}}

      :absent ->
        {:ok, {:absent, self}}

      {:collision, collision_hash} ->
        {:error,
         %CollisionError{
           data_hash: data_hash,
           collision_hash: collision_hash
         }}
    end
  end

  @spec drop!(Self.t(), hash()) ::
          {leaf_status, Self.t()} | {:error, CollisionError.t()}
        when leaf_status: :dropped | :absent
  def drop!(self, data_hash) do
    drop(self, data_hash)
    |> unwrap_result_or_raise!()
  end

  @doc """
  Recalculates any missing hashes throughout the tree.
  """
  @spec rehash(Self.t()) :: Self.t()
  def rehash(%Self{} = self) do
    %{self | root: rehash_subtree(self.root)}
  end

  @doc """
  Returns a proof of the presence or absence of the given hash in the tree.
  The proof consists of a list representing the path between the tree root
  to the leaf node for the data hash. Each path element is the hash of the
  sibling branch at that node.
  """
  @spec proof(Self.t(), hash()) ::
          {:ok, proof()}
          | {:error, StaleTreeError.t()}
          | {:error, CollisionError.t()}
  def proof(%Self{} = self, <<_::256>> = data_hash) do
    path = path_for_data_hash(data_hash, self.depth)

    with {:ok, _root_hash} <- root_hash(self) do
      case subtree_proof(self.root, path, data_hash) do
        {presence, proof} when presence in [:present, :absent] ->
          {:ok, {presence, proof}}

        {:collision, collision_hash} ->
          {:error,
           %CollisionError{
             data_hash: data_hash,
             collision_hash: collision_hash
           }}
      end
    end
  end

  @spec proof!(Self.t(), hash()) :: proof()
  def proof!(self, data_hash) do
    proof(self, data_hash)
    |> unwrap_result_or_raise!()
  end

  @doc """
  Checks the validity of a proof as returned by the proof/2 function. This
  works both for presence proofs and absence proofs.
  """
  @spec valid_proof?(hash(), proof(), hash()) :: boolean()
  def valid_proof?(
        <<_::256>> = data_hash,
        {presence, proof_path},
        <<_::256>> = root_hash
      )
      when presence in [:present, :absent] and is_list(proof_path) do
    max_tree_depth = Enum.count(proof_path)
    path = path_for_data_hash(data_hash, max_tree_depth)

    initial_hash =
      case presence do
        :present -> data_hash
        :absent -> @empty_hash
      end

    expected_root_hash =
      Enum.zip(path, proof_path)
      |> Enum.reverse()
      |> Enum.reduce(initial_hash, fn
        {:left, sibling_hash}, acc_hash -> hash(acc_hash <> sibling_hash)
        {:right, sibling_hash}, acc_hash -> hash(sibling_hash <> acc_hash)
      end)

    expected_root_hash == root_hash
  end

  @spec path_for_data_hash(binary(), non_neg_integer()) :: path()
  defp path_for_data_hash(data_hash, tree_depth) do
    <<
      path_bitstring::binary-unit(1)-size(tree_depth),
      _::bitstring
    >> = data_hash

    for <<bit::1 <- path_bitstring>> do
      case bit do
        0 -> :left
        1 -> :right
      end
    end
  end

  @spec put_at_path(tree(), path(), hash()) ::
          {:inserted, tree()} | :present | {:collision, hash()}
  defp put_at_path(:empty, [], data_hash) do
    {:inserted, {:leaf, data_hash}}
  end

  defp put_at_path(:empty, path, data_hash) do
    put_at_path({:no_hash, :empty, :empty}, path, data_hash)
  end

  defp put_at_path(
         {_branch_hash, left, right},
         [path_direction | path_tail],
         data_hash
       ) do
    case path_direction do
      :left ->
        with {:inserted, subtree} <- put_at_path(left, path_tail, data_hash) do
          {:inserted, {:no_hash, subtree, right}}
        end

      :right ->
        with {:inserted, subtree} <- put_at_path(right, path_tail, data_hash) do
          {:inserted, {:no_hash, left, subtree}}
        end
    end
  end

  defp put_at_path({:leaf, data_hash}, [], data_hash) do
    :present
  end

  defp put_at_path({:leaf, collision_hash}, [], data_hash)
       when collision_hash != data_hash do
    {:collision, collision_hash}
  end

  @spec drop_at_path(tree(), path(), hash()) ::
          {:dropped, tree()} | :absent | {:collision, hash()}
  defp drop_at_path(:empty, _path, _data_hash), do: :absent

  defp drop_at_path(
         {_branch_hash, left, right},
         [path_direction | path_tail],
         data_hash
       ) do
    collapse_if_empty = fn
      {:no_hash, :empty, :empty} -> :empty
      other -> other
    end

    case path_direction do
      :left ->
        with {:dropped, subtree} <- drop_at_path(left, path_tail, data_hash) do
          {:dropped, collapse_if_empty.({:no_hash, subtree, right})}
        end

      :right ->
        with {:dropped, subtree} <- drop_at_path(right, path_tail, data_hash) do
          {:dropped, collapse_if_empty.({:no_hash, left, subtree})}
        end
    end
  end

  defp drop_at_path({:leaf, data_hash}, [], data_hash) do
    {:dropped, :empty}
  end

  defp drop_at_path({:leaf, collision_hash}, [], data_hash)
       when collision_hash != data_hash do
    {:collision, collision_hash}
  end

  # Refreshes the cached hashes in the given subtree.
  @spec rehash_subtree(tree()) :: tree()
  defp rehash_subtree({:no_hash, left, right}) do
    new_left = rehash_subtree(left)
    new_right = rehash_subtree(right)

    {
      calculate_subtree_hash({:no_hash, left, right}),
      new_left,
      new_right
    }
  end

  defp rehash_subtree(subtree), do: subtree

  @spec subtree_hash(tree()) :: hash()
  defp subtree_hash(subtree) do
    case cached_subtree_hash(subtree) do
      {:ok, hash} -> hash
      :none -> calculate_subtree_hash(subtree)
    end
  end

  @spec calculate_subtree_hash(tree()) :: hash()
  defp calculate_subtree_hash({:no_hash, left, right}) do
    hash(subtree_hash(left) <> subtree_hash(right))
  end

  @spec cached_subtree_hash(tree()) :: {:ok, hash()} | :none
  defp cached_subtree_hash(:empty), do: {:ok, @empty_hash}

  defp cached_subtree_hash({:no_hash, _left, _right}), do: :none

  defp cached_subtree_hash({branch_hash, _left, _right})
       when is_binary(branch_hash),
       do: {:ok, branch_hash}

  defp cached_subtree_hash({:leaf, data_hash}), do: {:ok, data_hash}

  @spec cached_subtree_hash!(tree()) :: hash()
  defp cached_subtree_hash!(tree) do
    case cached_subtree_hash(tree) do
      {:ok, hash} -> hash
      :none -> raise %StaleTreeError{}
    end
  end

  @spec subtree_proof(tree(), path(), hash()) ::
          proof() | {:collision, hash()}
  defp subtree_proof(:empty, _path, _data_hash), do: {:absent, []}

  defp subtree_proof(
         {_branch_hash, left, right},
         [path_direction | path_tail],
         data_hash
       ) do
    {proof_path_element, subtree} =
      case path_direction do
        :left -> {cached_subtree_hash!(right), left}
        :right -> {cached_subtree_hash!(left), right}
      end

    case subtree_proof(subtree, path_tail, data_hash) do
      {presence, proof_subpath}
      when presence in [:present, :absent] and is_list(proof_subpath) ->
        {presence, [proof_path_element | proof_subpath]}

      {:collision, _collision_hash} = collision ->
        collision
    end
  end

  defp subtree_proof({:leaf, data_hash}, [], data_hash), do: {:present, []}

  defp subtree_proof({:leaf, collision_hash}, [], data_hash)
       when collision_hash != data_hash do
    {:collision, collision_hash}
  end

  @spec unwrap_result_or_raise!({:ok, out} | {:error, Exception.t()}) :: out
        when out: any()
  defp unwrap_result_or_raise!({:ok, out}), do: out
  defp unwrap_result_or_raise!({:error, e}) when is_exception(e), do: raise(e)
end
