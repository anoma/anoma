defmodule Anoma.TransparentResource.Delta do
  @moduledoc """
  delta functions. not a struct don't make it a struct
  """

  @behaviour Noun.Nounable.Kind

  @type t() :: %{binary() => integer()}

  defmodule DeltaError do
    defexception [:message]
  end

  def add(d1 = %{}, d2 = %{}) do
    Map.merge(d1, d2, fn _k, v1, v2 -> v1 + v2 end)
    |> make_sane()
  end

  def sub(d1 = %{}, d2 = %{}) do
    add(d1, negate(d2))
  end

  def negate(d = %{}) do
    for {k, v} <- d, into: %{} do
      {k, -v}
    end
  end

  def sane?(d = %{}) do
    !Enum.any?(d, fn {_, v} -> v == 0 end)
  end

  def sane!(d = %{}) do
    for {_, v} <- d do
      if v == 0 do
        raise DeltaError, "zero element found in delta"
      end
    end

    true
  end

  def make_sane(d = %{}) do
    Map.reject(d, fn {_k, v} -> v == 0 end)
  end

  # This would be automated by Noun.Nounable.Map.from_noun/1
  @spec from_noun(Noun.t()) :: {:ok, t()} | :error
  def from_noun(noun) do
    maybe_record =
      Enum.map(Noun.list_nock_to_erlang(noun), fn
        [x, [v_sign | v_value] | terminator]
        when terminator in [0, <<>>, <<0>>, []] ->
          {x,
           if Noun.is_zero(v_sign) do
             v_value
           else
             -v_value
           end}

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
