defmodule Anoma.TransparentResource.Delta do
  @moduledoc """
  delta functions. not a struct don't make it a struct
  """

  @type t() :: %{binary() => integer()}

  def add(d1 = %{}, d2 = %{}) do
    Map.merge(d1, d2, fn _k, v1, v2 -> v1 + v2 end)
    |> Map.reject(fn {_k, v} -> v == 0 end)
  end

  def sub(d1 = %{}, d2 = %{}) do
    Map.merge(d1, d2, fn _k, v1, v2 -> v1 - v2 end)
    |> Map.reject(fn {_k, v} -> v == 0 end)
  end
end