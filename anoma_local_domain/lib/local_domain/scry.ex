defmodule Anoma.LocalDomain.Scry do
  @moduledoc """
  Scrying via Elixir.

  Keyspaces:
    - /anoma/local/[unique id]/[time]/key: local storage
    - /anoma/controller/[node id]/[time]/key: Perform read-only tx on
      controller and cache
  """

  use Anoma.LocalDomain
  alias Anoma.LocalDomain.HandlerRegistry

  @spec scry([]) :: :absent
  def scry([]) do
    :absent
  end

  @spec scry(list()) :: {:ok, term()} | :absent | {:error, term()}
  def scry(path) when is_list(path) do
    scry_inner([], path)
  end

  def scry_inner(prev_prefixes, key) do
    with {matched_prefix, handler} <-
           HandlerRegistry.match(prev_prefixes, key) do
      prev_prefixes = prev_prefixes ++ [matched_prefix]
      key = Enum.drop(key, Enum.count(matched_prefix))
      handler.(prev_prefixes, key)
    else
      _ -> {:error, :no_handler}
    end
  end

  def scry_local(prev_prefixes, key) do
    # todo: local ids, time
    [_local_id, _time | subkey] = key
    # use the subkey's handler, if any
    with {:ok, result} <- scry_inner(prev_prefixes, subkey) do
      result
    else
      :absent ->
        :absent

      {:error, :no_handler} ->
        Anoma.LocalDomain.Storage.read_local(subkey)

      {:error, e} ->
        {:error, e}
    end
  end

  def scry_controller(_prev_prefixes, key) do
    # todo: get controller id, submit ro tx
    {:error, key}
  end
end
