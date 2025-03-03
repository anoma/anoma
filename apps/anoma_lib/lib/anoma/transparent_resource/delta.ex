defmodule Anoma.TransparentResource.Delta do
  @moduledoc """
  delta functions. not a struct don't make it a struct
  """

  @behaviour Noun.Nounable.Kind

  @type t() :: %{binary() => integer()}

  defmodule DeltaError do
    defexception [:message]
  end

  @spec add(t(), t()) :: t()
  def add(d1 = %{}, d2 = %{}) do
    Map.merge(d1, d2, fn _k, v1, v2 -> v1 + v2 end)
    |> make_sane()
  end

  @spec sub(t(), t()) :: t()
  def sub(d1 = %{}, d2 = %{}) do
    add(d1, negate(d2))
  end

  @spec negate(t()) :: t()
  def negate(d = %{}) do
    for {k, v} <- d, into: %{} do
      {k, -v}
    end
  end

  @spec sane?(t()) :: boolean()
  def sane?(d = %{}) do
    !Enum.any?(d, fn {_, v} -> v == 0 end)
  end

  @spec sane!(t()) :: true
  def sane!(d = %{}) do
    for {_, v} <- d do
      if v == 0 do
        raise DeltaError, "zero element found in delta"
      end
    end

    true
  end

  @spec make_sane(t()) :: t()
  def make_sane(d = %{}) do
    Map.reject(d, fn {_k, v} -> v == 0 end)
  end

  @spec from_noun(Noun.t()) :: {:ok, t()} | :error
  def from_noun(noun) do
    with {:ok, map} <- Noun.Nounable.Map.from_noun(noun) do
      res =
        map
        |> Enum.map(fn {key, value} -> {key, Noun.decode_signed(value)} end)
        |> Map.new()

      {:ok, res}
    else
      _ -> :error
    end
  end

  @spec to_noun(t()) :: Noun.t()
  def to_noun(delta) do
    Enum.map(delta, fn {k, signed_integer} ->
      {k, Noun.encode_signed(signed_integer)}
    end)
    |> Map.new(fn x -> x end)
    |> Noun.Nounable.to_noun()
  end
end
