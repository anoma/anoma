defmodule MapSetMap do
  @moduledoc """
  An IdentityMap where the values are MapSets, plus some convenient helper functions.
  """

  @type t(k, v) :: IdentityMap.t(k, MapSet.t(v))

  def new() do
    IdentityMap.new(MapSet.new(), &MapSetMap.empty_map_p/1)
  end

  def get(map, key) do
    IdentityMap.get(map, key)
  end

  # add value to the MapSet keyed by key
  def add(map, key, value) do
    IdentityMap.update(map, key, fn set -> MapSet.put(set, value) end)
  end

  # remove " from "
  def remove(map, key, value) do
    IdentityMap.update(map, key, fn set -> MapSet.delete(set, value) end)
  end

  # must be exported; only here so it can be pointed to
  def empty_map_p(x) do
    MapSet.size(x) == 0
  end
end
