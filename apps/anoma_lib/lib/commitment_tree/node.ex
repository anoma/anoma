defmodule CommitmentTree.Node do
  alias __MODULE__

  import Noun
  use TypedStruct

  typedstruct enforce: true do
    field(:hash, binary())

    # list of nodes or binary hashes--apparently cannot specify this in the type system?
    field(:children, tuple())

    # a hash denotes either a leaf or an empty subtree (in which case it will be key_zero)
  end

  @doc """
  Creates a new internal node.
  Children is a tuple of size spec.splay, each element of which is either a binary or another node.
  """
  @spec new(CommitmentTree.Spec.t(), tuple()) :: CommitmentTree.Node.t()
  def new(spec, children) do
    hash = CommitmentTree.Spec.hash_ref_to_hash(spec.hash)

    %CommitmentTree.Node{
      children: children,
      hash:
        hash.(
          map_tuple(children, fn x ->
            if is_binary(x) do
              x
            else
              x.hash
            end
          end)
        )
    }
  end

  @doc """
  Creates a new internal node, all children of which are empty.
  """
  @spec new_empty(CommitmentTree.Spec.t()) :: CommitmentTree.Node.t()
  def new_empty(spec) do
    new(spec, Tuple.duplicate(spec.key_zero, spec.splay))
  end

  @doc """
  Produces a proof for leaf #cursor of node, taking the form of a nested tuple,
  as described in proof.ex
  """
  @spec prove(CommitmentTree.Spec.t(), CommitmentTree.Node.t(), integer()) ::
          tuple()
  def prove(spec, node, cursor) do
    prove(spec, spec.splay_suff_prod, node, cursor)
  end

  defp prove(spec, suff_prod, node, cursor) do
    proof =
      map_tuple(node.children, fn x ->
        if is_binary(x) do
          x
        else
          x.hash
        end
      end)

    i =
      if suff_prod == [] do
        cursor
      else
        Integer.floor_div(cursor, hd(suff_prod))
      end

    child = elem(node.children, i)

    if is_binary(child) do
      proof
    else
      put_elem(
        proof,
        i,
        prove(spec, tl(suff_prod), child, Integer.mod(cursor, hd(suff_prod)))
      )
    end
  end

  # grumble
  # unlikely to have a splay >4 anyway, but it still rankles...
  defp map_tuple({x, y}, f) do
    {f.(x), f.(y)}
  end

  defp map_tuple({x, y, z}, f) do
    {f.(x), f.(y), f.(z)}
  end

  defp map_tuple({x, y, z, w}, f) do
    {f.(x), f.(y), f.(z), f.(w)}
  end

  defp map_tuple(xs, f) do
    xs
    |> Tuple.to_list()
    |> Enum.map(f)
    |> List.to_tuple()
  end

  defimpl Noun.Nounable, for: Node do
    @impl true
    def to_noun(node = %Node{}) do
      [
        [:erlang.byte_size(node.hash) | node.hash]
        | children_nounify(node.children)
      ]
    end

    defp children_nounify({a, b}) do
      left = children_nounify(a)
      right = children_nounify(b)

      [left | right]
    end

    defp children_nounify(bin) when is_binary(bin) do
      size = :erlang.byte_size(bin)
      [size | bin]
    end

    defp children_nounify(node = %Node{}) do
      to_noun(node)
    end
  end

  @spec from_noun(Noun.t()) :: {:ok, t()} | :error
  def from_noun([[size | hash], left_child_noun | right_child_noun]) do
    with {:ok, left_child} <- children_from_noun(left_child_noun),
         {:ok, right_child} <- children_from_noun(right_child_noun) do
      size = Noun.atom_binary_to_integer(size)

      {:ok,
       %Node{
         hash: Noun.atom_integer_to_binary(hash, size),
         children: {left_child, right_child}
       }}
    else
      _ -> :error
    end
  end

  @spec children_from_noun(Noun.t()) :: {:ok, t() | binary()} | :error
  defp children_from_noun(
         node = [[_size | _hash], _left_child | _right_child]
       ) do
    with {:ok, node} <- from_noun(node) do
      {:ok, node}
    else
      _ -> :error
    end
  end

  defp children_from_noun([size | bin])
       when is_noun_atom(size) and is_noun_atom(bin) do
    {:ok, Noun.atom_integer_to_binary(bin, Noun.atom_binary_to_integer(size))}
  end

  defp children_from_noun(_), do: :error
end
