defmodule Anoma.RM.Transparent.Primitive.DeltaHash do
  @spec delta_add(integer(), integer()) :: integer()
  def delta_add(delta1, delta2) do
    d1 = decode(delta1)
    d2 = decode(delta2)

    Map.merge(d1, d2, fn _k, v1, v2 -> v1 + v2 end)
    |> make_sane()
    |> hash()
  end

  @spec delta_sub(integer(), integer()) :: integer()
  def delta_sub(delta1, delta2) do
    d1 = decode(delta1)
    d2 = delta2 |> decode() |> negate() |> hash()

    delta_add(d1, d2)
  end

  @spec make_sane(map()) :: map()
  def make_sane(d = %{}) do
    Map.reject(d, fn {_k, v} -> v == 0 end)
  end

  @spec negate(map()) :: map()
  def negate(d) do
    for {k, v} <- d, into: %{} do
      {k, -v}
    end
  end

  @spec decode(integer()) :: map()
  def decode(delta) do
    {:ok, noun_map} = delta |> Noun.atom_integer_to_binary() |> Noun.Jam.cue()

    {:ok, map} = noun_map |> Noun.Nounable.Map.from_noun()

    map
  end

  @spec hash(map()) :: integer()
  def hash(delta) do
    delta
    |> Noun.Nounable.to_noun()
    |> Noun.Jam.jam()
    |> Noun.atom_binary_to_integer()
  end
end
