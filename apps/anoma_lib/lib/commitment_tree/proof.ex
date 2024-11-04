defmodule CommitmentTree.Proof do
  @moduledoc """
  I represent a compact proof that a particular element is contained within the
  commitment tree.
  """
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
    if depth == 0 do
      {spec.hash.(proof), cm == elem(proof, path)}
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

      {spec.hash.(put_elem(proof, i, hash)), valid}
    end
  end
end
