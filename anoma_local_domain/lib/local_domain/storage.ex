defmodule Anoma.LocalDomain.Storage do
  @moduledoc """
  Local storage subsystem. Timestamped, but not in an integrity-requiring way
  very much; using System.monotonic_time/0 (?)

  Writes to all subspaces, but the main API writes to /anoma/local/[local id]/.
  """

  def start_link(_) do

  end

  @doc """
  Writes to /anoma/local/[local id]/ at the current time.
  """
  def write_local(key, value) do

  end

  @doc """
  Writes to any possible key, including timestamp. For populating controller
  value cache.
  """
  def write_any(full_key, value) do

  end

  @doc """
  Reads from any possible key.
  """
  def read(full_key) do

  end

  @doc """
  Reads from any possible key, blocking if not a value or :absent.
  """
  def read_and_block(full_key) do

  end
end
