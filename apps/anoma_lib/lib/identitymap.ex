defmodule IdentityMap do
  @moduledoc """
  A map with an identity value; all keys not explicitly assigned a value map to
  the identity.
  """
  @enforce_keys [:map, :identity, :identityp]
  defstruct [:identity, :identityp, map: %{}]

  @type t(k, v) :: %__MODULE__{
          map: %{k => v},
          identity: k,
          identityp: function()
        }

  def new(from \\ %{}, identity, identityp) do
    %IdentityMap{
      map: Map.reject(from, fn {_k, v} -> identityp.(v) end),
      identity: identity,
      identityp: identityp
    }
  end

  def get(map, key) do
    Map.get(map.map, key, map.identity)
  end

  def put(map, key, value) do
    %{
      map
      | map:
          if map.identityp.(value) do
            Map.delete(map.map, key)
          else
            Map.put(map.map, key, value)
          end
    }
  end

  def update(map, key, fun) do
    put(map, key, fun.(get(map, key)))
  end
end
