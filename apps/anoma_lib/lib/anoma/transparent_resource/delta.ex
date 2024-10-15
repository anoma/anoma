defmodule Anoma.TransparentResource.Delta do
  @moduledoc """
  delta functions. not a struct don't make it a struct
  """

  @behaviour Noun.Nounable.Kind

  @type t() :: %{binary() => integer()}

  def add(d1 = %{}, d2 = %{}) do
    Map.merge(d1, d2, fn _k, v1, v2 -> v1 + v2 end)
    |> Map.reject(fn {_k, v} -> v == 0 end)
  end

  def sub(d1 = %{}, d2 = %{}) do
    Map.merge(d1, d2, fn _k, v1, v2 -> v1 - v2 end)
    |> Map.reject(fn {_k, v} -> v == 0 end)
  end

  # This would be automated by Noun.Nounable.Map.from_noun/1
  @spec from_noun(Noun.t()) :: {:ok, t()} | :error
  def from_noun(noun) do
    maybe_record =
      Enum.map(Noun.list_nock_to_erlang(noun), fn
        [x, y | terminator] when terminator in [0, <<>>, <<0>>, []] ->
          {x, y}

        _ ->
          :error
      end)

    if Enum.any?(maybe_record, &(:error == &1)) do
      :error
    else
      {:ok, Map.new(maybe_record)}
    end
  end
end
