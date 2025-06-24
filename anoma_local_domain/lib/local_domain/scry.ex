defmodule Anoma.LocalDomain.Scry do
  @moduledoc """
  Scrying via Elixir.

  Keyspaces:
    - /anoma/local/[unique id]/[time]/key: local storage
    - /anoma/controller/[node id]/[time]/key: Perform read-only tx on
      controller and cache
  """

  @spec scry([]) :: :absent
  def scry([]) do
    :absent
  end

  @spec scry(list()) :: {:ok, term()} | :absent | {:error, term()}
  def scry(path) when is_list(path) do
    scry_inner([], path)
  end

  def scry_inner(prev_prefixes, key) do
    {matched_prefix, handler} =
      Anoma.LocalDomain.HandlerRegistry.match(prev_prefixes, key)
    prev_prefixes = prev_prefixes ++ [matched_prefix]
    key = Enum.drop(key, Enum.count(matched_prefix))
    handler.(prev_prefixes, key)
  end

  def scry_local(_prev_prefixes, key) do
    # todo: local ids
    # todo: time
    # todo: yield to application subspaces
    [_local_id, _time | rest] = key
    Anoma.LocalDomain.Storage.read_local(rest)
  end

  def scry_controller(_prev_prefixes, key) do
    # todo: get controller id, submit ro tx
    {:error, key}
  end
end
