defmodule Anoma.Delta do
  @moduledoc false

  # usually non_neg_integer, but not in execution
  @type t() :: %{binary() => integer()}

  @spec add(t(), t()) :: t()
  def add(d1, d2) do
    Map.merge(d1, d2, fn _k, v1, v2 -> v1 + v2 end)
    |> Map.reject(fn {_k, v} -> v == 0 end)
  end

  @spec sub(t(), t()) :: t()
  # todo: subtract from zero correctly
  def sub(d1, d2) do
    Map.merge(d1, d2, fn _k, v1, v2 -> v1 - v2 end)
    |> Map.reject(fn {_k, v} -> v == 0 end)
  end

  # use nock map once it exists
  def to_noun(delta = %{}) do
    delta_list = for {k, v} <- delta do
      if v >= 0 do
        [k, 0 | v]
      else
        [k, 1 | v]
      end
    end
    Noun.list_e2n(delta_list)
  end
end
