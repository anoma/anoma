defprotocol Noun.Nounable do
  @doc """
  I turn the transaction into a noun
  """
  @spec to_noun(t()) :: Noun.t()
  def to_noun(transaction)
end

defmodule Noun.Nounable.Kind do
  @doc """
  I convert the given `t:Noun.t/0` into the given structure
  """
  @callback from_noun(noun :: Noun.t()) :: {:ok, any()} | :error
end

alias Noun.Nounable
alias Noun.Nounable.Kind

defimpl Nounable, for: List do
  @moduledoc """
  I offer an implementation of Nounable and from_noun for lists
  """
  def to_noun([]), do: 0
  def to_noun([h | t]), do: [Nounable.to_noun(h) | Nounable.to_noun(t)]

  @behaviour Kind

  @doc """
  I convert the given Noun into a list of nouns.

  I do not recursively convert the structures in the list.
  """
  @spec from_noun(Noun.t()) :: {:ok, list(Noun.t())} | :error
  def from_noun(0), do: {:ok, []}
  def from_noun([]), do: {:ok, []}

  def from_noun([h | t]) do
    with {:ok, ts} <- from_noun(t) do
      {:ok, [h | ts]}
    end
  end

  def from_noun(_) do
    :error
  end
end

defimpl Nounable, for: Integer do
  def to_noun(x) when x >= 0, do: x
  # We should support this in time?
  def to_noun(x),
    do: raise(ArgumentError, message: "The value #{inspect(x)} is negative")
end

defimpl Nounable, for: Map do
  # use nock map once it exists
  def to_noun(map) do
    for {k, v} <- map do
      [Nounable.to_noun(k) | Nounable.to_noun(v)]
    end ++ 0
  end

  # ditto here
  @behaviour Kind
  @doc """
  I convert the given Noun into a map of nouns.

  I do not recursively convert the structures in the list.
  """

  @spec from_noun(Noun.t()) :: {:ok, %{Noun.t() => Noun.t()}} | :error
  def from_noun(noun) do
    with {:ok, value} <- from_noun_int(noun) do
      {:ok, Map.new(value)}
    end
  end

  defp from_noun_int(0), do: {:ok, []}
  defp from_noun_int([]), do: {:ok, []}

  defp from_noun_int([[k | v] | t]) do
    with {:ok, ts} <- from_noun_int(t) do
      {:ok, [{k, v} | ts]}
    end
  end

  defp from_noun_int(_), do: :error
end

defimpl Nounable, for: BitString do
  def to_noun(binary), do: binary
end

defimpl Nounable, for: Atom do
  def to_noun(true), do: 0
  def to_noun(false), do: 1
  def to_noun(atom), do: Atom.to_string(atom)
end

defimpl Nounable, for: Tuple do
  def to_noun({}), do: 0

  def to_noun(tuple) do
    size = tuple_size(tuple)

    Nounable.to_noun(:erlang.element(size, tuple))
    |> to_noun_index(tuple, size - 1)
  end

  @spec to_noun_index(any(), tuple(), non_neg_integer()) :: any()
  def to_noun_index(nock_list, _tuple, 0), do: nock_list

  def to_noun_index(append_onto, tuple, index) do
    [Nounable.to_noun(:erlang.element(index, tuple)) | append_onto]
    |> to_noun_index(tuple, index - 1)
  end

  @behaviour Kind

  @doc """
  I convert the given Noun into a list of nouns.

  I do not recursively convert the structures in the list.

  This can't fail, so we don't return :error
  """
  @spec from_noun(Noun.t()) :: {:ok, tuple()}
  def from_noun(noun), do: :erlang.list_to_tuple(from_noun_internal(noun))

  @spec from_noun_internal(Noun.t()) :: list()
  def from_noun_internal([h | t]), do: [h | from_noun_internal(t)]
  def from_noun_internal([]), do: [0]
  def from_noun_internal(x), do: [x]
end
