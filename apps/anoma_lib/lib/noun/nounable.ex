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
  import Noun

  @moduledoc """
  I offer an implementation of Nounable and from_noun for lists
  """
  def to_noun([]), do: 0
  def to_noun([h | t]), do: [Nounable.to_noun(h) | Nounable.to_noun(t)]

  @behaviour Kind

  import Noun

  @doc """
  I convert the given Noun into a list of nouns.

  I do not recursively convert the structures in the list.
  """
  @spec from_noun(Noun.t()) :: {:ok, list(Noun.t())} | :error
  def from_noun(zero) when is_noun_zero(zero), do: {:ok, []}

  def from_noun([h | t]) do
    with {:ok, ts} <- from_noun(t) do
      {:ok, [h | ts]}
    end
  end

  def from_noun(_) do
    :error
  end
end

defimpl Nounable, for: Bool do
  import Noun
  @behaviour Kind

  def to_noun(bool) when bool in [true, false] do
    Nounable.to_noun(bool)
  end

  def to_noun(), do: :error

  @doc """
  I convert the given Nock boolean to Elixir boolean.
  """
  @spec from_noun(Noun.t()) :: {:ok, bool()} | :error
  def from_noun(zero) when is_noun_zero(zero), do: {:ok, true}

  def from_noun(one) when one in [1, <<1>>], do: {:ok, false}

  def from_noun(_), do: :error
end

defimpl Nounable, for: Integer do
  def to_noun(x) when x >= 0, do: x
  # We should support this in time?
  def to_noun(x),
    do: raise(ArgumentError, message: "The value #{inspect(x)} is negative")
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

defimpl Nounable, for: MapSet do
  import Noun

  @doc """
  Given a set, I convert it to a Nock set, i.e. a mug-balanced tree.
  """
  @spec to_noun(MapSet.t()) :: Noun.t()
  def to_noun(set) do
    with [hd | tail] <- set |> MapSet.to_list() do
      for elem <- tail, reduce: [Nounable.to_noun(hd), 0 | 0] do
        acc -> noun_set_put(acc, Nounable.to_noun(elem))
      end
    else
      _ -> 0
    end
  end

  def noun_set_put(zero, elem) when is_noun_zero(zero) do
    [elem, 0 | 0]
  end

  def noun_set_put([node | [left | right]], elem) do
    if Noun.Order.gor(elem, node) do
      c = [node_c | [left_c | right_c]] = noun_set_put(left, elem)

      if Noun.Order.mor(node, node_c) do
        [node, c | right]
      else
        [node_c, left_c, node, right_c | right]
      end
    else
      c = [node_c | [left_c | right_c]] = noun_set_put(right, elem)

      if Noun.Order.mor(node, node_c) do
        [node, left | c]
      else
        [node_c, [node, left | left_c] | right_c]
      end
    end
  end

  @behaviour Kind
  @doc """
  I convert the given Noun of set type in to a set of nouns.
  """
  @spec from_noun(Noun.t()) :: {:ok, MapSet.t()} | :error
  def from_noun(tree) do
    with {:ok, list} <- parse_tree_nodes(tree, []) do
      {:ok, list |> MapSet.new()}
    else
      _ -> :error
    end
  end

  @spec parse_tree_nodes(Noun.t(), list(Noun.t())) ::
          {:ok, list(Noun.t())} | :error
  defp parse_tree_nodes([node | [left | right]], acc) do
    with {:ok, left_acc} <- parse_tree_nodes(left, []),
         {:ok, right_acc} <- parse_tree_nodes(right, []) do
      full_acc = acc ++ left_acc ++ right_acc

      {:ok, [node | full_acc]}
    else
      _ -> :error
    end
  end

  defp parse_tree_nodes(_zero, acc) do
    {:ok, acc}
  end
end

defimpl Nounable, for: Map do
  def to_noun(map) do
    for {k, v} <- map do
      [Nounable.to_noun(k) | Nounable.to_noun(v)]
    end
    |> MapSet.new()
    |> Nounable.to_noun()
  end

  # ditto here
  @behaviour Kind
  @doc """
  I convert the given Noun into a map of nouns.

  I do not recursively convert the structures in the list.
  """

  @spec from_noun(Noun.t()) :: {:ok, %{Noun.t() => Noun.t()}} | :error
  def from_noun(noun) do
    with {:ok, value} <- Nounable.MapSet.from_noun(noun) do
      {:ok, Map.new(value, fn [x | y] -> {x, y} end)}
    end
  end
end
