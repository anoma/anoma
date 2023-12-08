defmodule Anoma.Delta do
  @moduledoc false

  @type t() :: %{binary() => non_neg_integer()}

  def add(d1, d2) do
    Map.merge(d1, d2, fn _k, v1, v2 -> v1 + v2 end)
    |> Map.reject(fn {_k, v} -> v == 0 end)
  end

  def sub(d1, d2) do
    Map.merge(d1, d2, fn _k, v1, v2 -> v1 - v2 end)
    |> Map.reject(fn {_k, v} -> v == 0 end)
  end
end
