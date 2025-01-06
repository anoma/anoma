defmodule Noun.Order do
  @moduledoc """
  I am the module implementing noun ordering functionality.

  In particular, I implement functions such as `dor/2`, `gor/2` and `mor/2`
  that provide a notion of partial ordering between nouns.
  """

  import Noun

  @doc """
  I am the mor function. I provide fast partial ordering on doubly-mugged
  nouns.
  """
  @spec mor(Noun.t(), Noun.t()) :: bool()
  def mor(a, b) do
    c = a |> Noun.mug() |> Noun.mug()
    d = b |> Noun.mug() |> Noun.mug()

    if c == d do
      dor(c, d)
    else
      c <= d
    end
  end

  @doc """
  I am the gor function. I provide fast partial ordering on mugged nouns.
  """
  @spec gor(Noun.t(), Noun.t()) :: bool()
  def gor(a, b) do
    c = Noun.mug(a)
    d = Noun.mug(b)

    if c == d do
      dor(c, d)
    else
      c <= d
    end
  end

  @doc """
  I am the dor function. I provide a partial ordeirng on bear nouns.

  When both nouns are atoms, I check whether the first is less than or
  equal than the second.

  If both are cells, I recurse on the heads unless they are the same.
  Otherwise I recurse on the tails.

  Otherwise, I consider the cell the greater noun.
  """
  @spec dor(Noun.t(), Noun.t()) :: bool()
  def dor(a, b) when is_noun_atom(a) and is_noun_atom(b) do
    Noun.atom_binary_to_integer(a) <= Noun.atom_binary_to_integer(b)
  end

  def dor([a_hd | a_tl], [b_hd | b_tl]) do
    unless a_hd == b_hd do
      dor(a_hd, b_hd)
    else
      dor(a_tl, b_tl)
    end
  end

  def dor(a, _b) when is_noun_cell(a) do
    false
  end

  def dor(_a, _b) do
    true
  end
end
