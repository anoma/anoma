defmodule CommitmentTree do
  @moduledoc """
  A simple commitment tree.

  Currently stores all commitments forever, and stores the full tree in memory,
  pending future more sophisticated retention policies.  Does not yet store any
  anchors itself for the same reason--this is a complex policy level decision.

  Has a fixed depth.

  Fiats that empty subtrees have a hash of 0 for simplicity.
  """
  alias Anoma.Node.Tables

  use TypedStruct

  typedstruct enforce: true do
    # the specification for this tree
    field(:spec, CommitmentTree.Spec.t())
    # the root node of the tree
    field(:root, CommitmentTree.Node.t())
    # the current number of commitments in the tree
    field(:size, integer())

    # the name of the mnesia table where the commitments will be stored.  may be nil
    field(:table, term())
  end

  ############################################################
  #                         Public Functions                 #
  ############################################################

  @spec init_storage(String.t()) :: :ok
  def init_storage(node_id \\ "") do
    {:ok, _} = Tables.initialize_tables_for_node(node_id)

    :ok
  end

  @doc """
  Creates a new `CommitmentTree` struct.
  """
  @spec new(CommitmentTree.Spec.t(), term()) :: CommitmentTree.t()
  def new(spec, table) do
    # create an empty tree
    tree = %CommitmentTree{
      spec: spec,
      size: 0,
      # technically, this gives us an initial anchor of H(zero, zero, zero...)
      # instead of zero, but it simplifies the logic, and you can't prove
      # anything against an empty tree anyway
      root: CommitmentTree.Node.new_empty(spec),
      table: table
    }

    if table do
      # read all the commitments out of the table and replay them into the tree
      # there seems to not be a better way to do this
      n = :mnesia.table_info(table, :size)

      {:atomic, cms} =
        :mnesia.transaction(fn ->
          Enum.map(
            0..(n - 1)//1,
            fn i ->
              :mnesia.read(table, i)
              |> hd
              |> elem(2)
            end
          )
        end)

      {tree, _anchor} = add_mem(tree, cms, n)
      tree
    else
      tree
    end
  end

  @doc """
  Adds commitments to the commitment tree, and returns the new tree and the anchor.
  TODO handle the tree's filling up
  """
  @spec add(CommitmentTree.t(), list(binary())) :: tuple()
  def add(tree, cms) do
    n = length(cms)

    if tree.table do
      add_mnesia(tree, cms)
    end

    add_mem(tree, cms, n)
  end

  defp add_mnesia(tree, cms) do
    :mnesia.transaction(fn ->
      Enum.reduce(cms, tree.size, fn cm, i ->
        :mnesia.write({tree.table, i, cm})
        i + 1
      end)
    end)
  end

  defp add_mem(tree, cms, n) do
    new_size = tree.size + n

    tree = %CommitmentTree{
      tree
      | size: new_size,
        root:
          addx(
            tree.spec,
            tree.root,
            tree.size,
            tree.spec.splay_suff_prod,
            cms,
            n
          )
    }

    {tree, tree.root.hash}
  end

  @spec prove(CommitmentTree.t(), integer()) :: CommitmentTree.Proof.t()
  def prove(tree, i) do
    # cannot use Integer.digits because we need to pad this out to depth digits
    {_, path} =
      Enum.reduce(1..tree.spec.depth, {i, []}, fn _, {i, acc} ->
        {Integer.floor_div(i, tree.spec.splay),
         [Integer.mod(i, tree.spec.splay) | acc]}
      end)

    path = path |> Enum.reverse() |> Integer.undigits(tree.spec.splay)

    CommitmentTree.Proof.new(
      path,
      CommitmentTree.Node.prove(tree.spec, tree.root, i)
    )
  end

  # missing from standard library for some reason?
  defp ceil_div(dividend, divisor) do
    Integer.floor_div(dividend + divisor - 1, divisor)
  end

  # cms may be longer than n; we just add the first n commitments
  # we could use take when recursing to avoid this, but that would spuriously cons a lot
  @spec addx(
          CommitmentTree.Spec.t(),
          CommitmentTree.Node.t(),
          integer(),
          list(integer()),
          list(binary()),
          integer()
        ) :: CommitmentTree.Node.t()
  defp addx(spec, node, cursor, suff_prod, cms, n) do
    children = node.children
    # no more recursion to be done; the children of this node are leaves
    if suff_prod == [] do
      CommitmentTree.Node.new(
        spec,
        Enum.reduce(
          Enum.zip(cms, cursor..(cursor + n - 1)),
          node.children,
          fn {cm, i}, hashes -> put_elem(hashes, i, cm) end
        )
      )
    else
      # offset within the first partly-already-full child to which new leaves will be added
      partial_child_offset = Integer.mod(cursor, hd(suff_prod))
      # index of the first child to which we add new leaves
      child = Integer.floor_div(cursor, hd(suff_prod))

      {children, child, cms, n} =
        if partial_child_offset != 0 do
          # how many leaves do we add to the first child?
          partial_child_nleaves = min(n, hd(suff_prod) - partial_child_offset)

          children =
            put_elem(
              children,
              child,
              addx(
                spec,
                elem(children, child),
                partial_child_offset,
                tl(suff_prod),
                cms,
                partial_child_nleaves
              )
            )

          {children, child + 1, Enum.drop(cms, partial_child_nleaves),
           n - partial_child_nleaves}
        else
          {children, child, cms, n}
        end

      # add all the new children
      # need // 1 to ensure that this is an ascending range, because we need it to be empty if n is 0
      {children, _, 0} =
        Enum.reduce(
          child..(child + ceil_div(n, hd(suff_prod)) - 1)//1,
          {children, cms, n},
          fn i, {children, cms, n} ->
            taken = min(n, hd(suff_prod))

            {put_elem(
               children,
               i,
               addx(
                 spec,
                 CommitmentTree.Node.new_empty(spec),
                 0,
                 tl(suff_prod),
                 cms,
                 taken
               )
             ), Enum.drop(cms, taken), n - taken}
          end
        )

      CommitmentTree.Node.new(spec, children)
    end
  end
end
