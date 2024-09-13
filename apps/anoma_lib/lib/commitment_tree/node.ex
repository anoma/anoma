defmodule CommitmentTree.Node do
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
    %CommitmentTree.Node{
      children: children,
      hash:
        spec.hash.(
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
end
