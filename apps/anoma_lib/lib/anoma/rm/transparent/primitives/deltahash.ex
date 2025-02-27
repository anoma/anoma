defmodule Anoma.RM.Transparent.Primitive.DeltaHash do
  @moduledoc """
  I am the Delta Hash module for the TRM.

  I provide the core interfaces necessary for the delta manipulation.

  ### Public API

  I have the following public functionality:

  - `hash/1`
  - `decode/1`
  - `delta_sub/2`
  - `delta_add/2`
  """

  @doc """
  I am the delta add function.

  Given two deltas, I decode them into their underlying maps, merge them
  appropriately, and hash bringing them to the integer format.
  """
  @spec delta_add(integer(), integer()) :: integer()
  def delta_add(delta1, delta2) do
    d1 = decode(delta1)
    d2 = decode(delta2)

    Map.merge(d1, d2, fn _k, v1, v2 -> v1 + v2 end)
    |> make_sane()
    |> hash()
  end

  @doc """
  I am the delta sub function.

  Given two deltas, I decode them into their underlying maps, negate the
  second argument and then merge them appropriately, hashing the result.
  """
  @spec delta_sub(integer(), integer()) :: integer()
  def delta_sub(delta1, delta2) do
    d2 = delta2 |> decode() |> negate() |> hash

    delta_add(delta1, d2)
  end

  @doc """
  I am the delta decode function.

  Given an integer, I cue it to see the underlying noun. Afterwards, I put
  it into the Elixir map format.
  """
  @spec decode(integer()) :: map()
  def decode(delta) do
    {:ok, noun_map} = delta |> Noun.atom_integer_to_binary() |> Noun.Jam.cue()

    {:ok, map} = noun_map |> Noun.Nounable.Map.from_noun()

    Enum.into(map, %{}, fn {k, v} -> {k, Noun.decode_signed(v)} end)
  end

  @doc """
  Given a delta-preimage - i.e. a map - I make it into a noun, jamming the
  result.
  """
  @spec hash(map()) :: integer()
  def hash(delta) do
    delta
    |> Enum.into(%{}, fn {k, v} -> {k, Noun.encode_signed(v)} end)
    |> Noun.Nounable.to_noun()
    |> Noun.Jam.jam()
    |> Noun.atom_binary_to_integer()
  end

  @spec make_sane(map()) :: map()
  defp make_sane(d = %{}) do
    Map.reject(d, fn {_k, v} -> v == 0 end)
  end

  @spec negate(map()) :: map()
  defp negate(d) do
    for {k, v} <- d, into: %{} do
      {k, -v}
    end
  end
end
