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

  @typep key() :: any()
  @typep value() :: any()

  @spec new(%{key() => value()}, value(), (value() -> boolean())) ::
          t(key(), value())
  def new(from \\ %{}, identity, identityp) do
    %IdentityMap{
      map: Map.reject(from, fn {_k, v} -> identityp.(v) end),
      identity: identity,
      identityp: identityp
    }
  end

  @spec get(t(key(), value()), key()) :: value()
  def get(map, key) do
    Map.get(map.map, key, map.identity)
  end

  @spec put(t(key(), value()), key(), value()) :: t(key(), value())
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

  @spec update(t(key(), value()), key(), (value() -> value())) :: term()
  def update(map, key, fun) do
    put(map, key, fun.(get(map, key)))
  end
end
