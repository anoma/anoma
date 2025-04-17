defmodule CommitmentTree.Proof do
  @moduledoc """
  I represent a compact proof that a particular element is contained within the
  commitment tree.
  """
  alias __MODULE__

  import Noun
  use TypedStruct

  # Not sure if there is a nice way to represent this for sending over the wire
  # (ok, I do, but it involves mutation), but for elixir, represent this is as:
  # a path, which is the digit-reversed (in base splay) offset of the
  # commitment, and a proof, which is a nested tuple structure; it has splay
  # elements, all but one of which are binaries, and one of which is itself a
  # nested tuple, except at the bottom level
  # the path is not strictly necessary, but it simplifies the logic
  typedstruct enforce: true do
    field(:path, integer())
    field(:proof, tuple())
  end

  @spec new(integer(), tuple()) :: t()
  def new(path, proof) do
    %CommitmentTree.Proof{path: path, proof: proof}
  end

  # does proof prove that commitment was a part of anchor?
  @spec verify(
          CommitmentTree.Spec.t(),
          CommitmentTree.Proof.t(),
          binary(),
          binary()
        ) :: boolean()
  def verify(spec, proof, anchor, commitment) do
    {hash, verified} =
      verifyx(spec, spec.depth - 1, proof.path, proof.proof, commitment)

    hash == anchor && verified
  end

  @spec verifyx(
          CommitmentTree.Spec.t(),
          integer(),
          integer(),
          tuple(),
          binary()
        ) :: {binary(), boolean()}
  def verifyx(spec, depth, path, proof, cm) do
    hash_fun = CommitmentTree.Spec.hash_ref_to_hash(spec.hash)

    if depth == 0 do
      {hash_fun.(proof), cm == elem(proof, path)}
    else
      i = Integer.mod(path, spec.splay)

      {hash, valid} =
        verifyx(
          spec,
          depth - 1,
          Integer.floor_div(path, spec.splay),
          elem(proof, i),
          cm
        )

      {hash_fun.(put_elem(proof, i, hash)), valid}
    end
  end

  defimpl Noun.Nounable, for: Proof do
    @impl true
    def to_noun(prf = %Proof{}) do
      [
        prf.path
        | to_sized_tuple(prf.proof)
      ]
    end

    defp to_sized_tuple({a, b}) do
      size_b = :erlang.byte_size(b)
      [to_sized_tuple(a), size_b | b]
    end

    defp to_sized_tuple(a) do
      size = :erlang.byte_size(a)
      [size | a]
    end
  end

  @spec from_noun(Noun.t()) :: :error | {:ok, t()}
  def from_noun([path | proof]) do
    with {:ok, tuple} <- from_sized_tuple(proof) do
      {:ok,
       %__MODULE__{
         path: Noun.atom_binary_to_integer(path),
         proof: tuple
       }}
    else
      _ -> :error
    end
  end

  @spec from_sized_tuple(Noun.t()) :: {:ok, tuple() | binary()} | :error
  defp from_sized_tuple([a, b_size | b]) do
    with {:ok, maybe_tuple} <- from_sized_tuple(a) do
      {:ok, {maybe_tuple, Noun.atom_integer_to_binary(b, b_size)}}
    else
      _ -> :error
    end
  end

  defp from_sized_tuple([size | atom])
       when is_noun_atom(size) and is_noun_atom(atom) do
    {:ok, Noun.atom_integer_to_binary(atom, size)}
  end

  defp from_sized_tuple(_), do: :error
end
