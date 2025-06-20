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
  def scry(path) do
    # registry mechanism:
    # register a prefix with your local domain to be handled by some fun
  end

  def scry_local(id, key) do
    # read from local storage
  end

  def scry_controller(node_id, key) do
    # submit ro tx
  end
end
