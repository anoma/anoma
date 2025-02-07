defmodule Anoma.RM.Transparent.Primitive.CommitmentAccumilator do
  @doc """
  I am the commitment accumulator add function for the transparent resource
  machine.

  Given the commitment set, I add a commitment to it.
  """

  @spec add(MapSet.t(), binary()) :: MapSet.t()
  def add(acc, cm) do
    MapSet.put(acc, cm)
  end

  @doc """
  I am the commitment accumulator witness function for the transparent
  resource machine.

  Given the commitment set and a commitment, I return the original set if
  the commitment is a member of the former. Otherwise, I return nil
  """

  @spec witness(MapSet.t(), binary()) :: MapSet.t() | nil
  def witness(acc, cm) do
    if MapSet.member?(acc, cm) do
      acc
    end
  end

  @doc """
  I am the commitment accumulator value function for the transparent
  resource machine.
  """

  @spec value(MapSet.t()) :: binary()
  def value(acc) do
    jammed_set = acc |> Noun.Nounable.to_noun() |> Noun.Jam.jam()
    jammed_set |> Noun.atom_binary_to_integer()
  end

  @doc """
  I am the commitment accumulator verify function for the transparent
  resource machine.

  Given the commitment, a witness (i.e. a set) and a commitment value, I
  output true iff the witness's value is the same as the provided value and
  the commitment is indeed in the set.
  """

  @spec verify(binary(), MapSet.t(), binary()) :: bool()
  def verify(cm, w, val) do
    val == value(w) and MapSet.member?(w, cm)
  end
end
