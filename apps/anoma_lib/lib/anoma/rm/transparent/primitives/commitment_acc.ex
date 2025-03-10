defmodule Anoma.RM.Transparent.Primitive.CommitmentAccumulator do
  # Pending: due to size issues, it would probably be best to use a fixed-size merkle tree
  @moduledoc """
  I am the commitment accumulator module for the TRM.

  I provide the basic interface to interact with the accumulators. The choice
  of the accumultor for the transparent case is just a set.

  ### Public API

  I provide the following public functionality:

  - `value/1`
  - `add/2`
  - `witness/2`
  - `verify/3`
  """

  @doc """
  I am the commitment accumulator add function for the transparent resource
  machine.

  Given the commitment set, I add a commitment to it.
  """

  @spec add(MapSet.t(), integer()) :: MapSet.t()
  def add(acc, cm) do
    MapSet.put(acc, cm)
  end

  @doc """
  I am the commitment accumulator witness function for the transparent
  resource machine.

  Given the commitment set and a commitment, I return the original set if
  the commitment is a member of the former. Otherwise, I return nil
  """

  @spec witness(MapSet.t(), integer()) :: MapSet.t() | nil
  def witness(acc, cm) do
    if MapSet.member?(acc, cm) do
      acc
    end
  end

  @doc """
  I am the commitment accumulator value function for the transparent
  resource machine.
  """

  @spec value(MapSet.t()) :: integer()
  def value(acc) do
    jammed_set = acc |> Noun.Nounable.to_noun() |> Noun.Jam.jam()
    jammed_set |> Noun.atom_binary_to_integer()
  end

  @doc """
  I am the commitment accumulator verify function for the transparent
  resource machine.

  Given the commitment, a witness (i.e. a set) and an accumulator value, I
  output true iff the witness's value is the same as the provided value and
  the commitment is indeed in the set.
  """

  @spec verify(integer(), MapSet.t(), integer()) :: boolean()
  def verify(cm, w, val) do
    val == value(w) and
      (MapSet.member?(w, Noun.atom_binary_to_integer(cm)) or
         MapSet.member?(w, Noun.atom_integer_to_binary(cm)))
  end
end
