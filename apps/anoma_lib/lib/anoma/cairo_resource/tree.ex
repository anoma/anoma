defmodule Anoma.CairoResource.Tree do
  @moduledoc """
  I represent a resource merkle tree.
  The resource tree should be quite small, such as with a depth of 4
  """

  use TypedStruct

  typedstruct enforce: true do
    # The resource merkle tree
    field(:tree, CommitmentTree.t())
    # The merkle root of resources in action
    field(:root, <<_::256>>)
    # The tree leaves: help find the target index
    field(:leaves, list(<<_::256>>))
  end

  @spec construct(CommitmentTree.Spec.t(), list(binary())) :: t()
  def construct(spec, leaves) do
    empty_rt = CommitmentTree.new(spec, nil)
    {rt, anchor} = CommitmentTree.add(empty_rt, leaves)

    %__MODULE__{
      tree: rt,
      root: anchor,
      leaves: leaves
    }
  end

  @spec prove(t(), binary()) :: list() | nil
  def prove(rt, leaf) do
    case Enum.find_index(rt.leaves, fn x -> x == leaf end) do
      nil ->
        nil

      index ->
        proof = CommitmentTree.prove(rt.tree, index)

        {_, _, path} =
          Enum.reduce(
            1..4,
            {proof.path, proof.proof, []},
            fn _, {path, proof, acc} ->
              {Integer.floor_div(path, 2), elem(proof, Integer.mod(path, 2)),
               [
                 {elem(proof, Integer.mod(path + 1, 2))
                  |> :binary.bin_to_list()
                  |> Cairo.felt_to_string(), Integer.mod(path, 2) == 1}
                 | acc
               ]}
            end
          )

        path
    end
  end

  @spec set_path(binary(), list()) :: :error | {:ok, binary()}
  def set_path(json, path, path_name \\ "merkle_path") do
    with {:ok, decoded_json} <-
           Jason.decode(json, objects: :ordered_objects),
         path_json_ob =
           path
           |> Enum.map(fn {f, s} ->
             Jason.OrderedObject.new([{"fst", f}, {"snd", s}])
           end) do
      put_in(decoded_json[path_name], path_json_ob) |> Jason.encode()
    else
      _ -> :error
    end
  end
end
