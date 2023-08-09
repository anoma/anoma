defmodule Taiga.VP.Trivial do
  @moduledoc """
  I represent a trivial Taiga Validity Predicate.

  I take 2 input notes and 2 output notes, and create a simple
  verifier circuit
  """

  use TypedStruct

  alias Taiga.{Base, Note, Note.Commitment, Nullifier}
  alias __MODULE__

  typedstruct do
    field(:owned_note_pub_id, Base.t(), default: Base.default())
    field(:input_notes, list(Note.t()), default: [Note.default(), Note.default()])
    field(:output_notes, list(Note.t()), default: [Note.default(), Note.default()])
  end

  @doc """
  I create a dummy trivial VP.
  """
  @spec dummy() :: t()
  def dummy() do
    %Trivial{}
  end

  @doc """
  Ι create an input verifier circuit, I take a note and two input and
  output notes
  """
  @spec input(Note.t(), list(Note.t()), list(Note.t())) :: t()
  def input(note, inputs, outputs) do
    owned = note |> Note.nullifier() |> Nullifier.inner()
    %Trivial{owned_note_pub_id: owned, input_notes: inputs, output_notes: outputs}
  end

  @doc """
  Ι create an input verifier circuit, I take a note and two input and
  output notes
  """
  @spec output(Note.t(), list(Note.t()), list(Note.t())) :: t()
  def output(note, inputs, outputs) do
    owned = note |> Note.commitment() |> Commitment.x()
    %Trivial{owned_note_pub_id: owned, input_notes: inputs, output_notes: outputs}
  end
end
