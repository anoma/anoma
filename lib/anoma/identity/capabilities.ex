defmodule Anoma.Identity.Capabilities do
  @moduledoc """
  I specify which capabilities to request when generating a new
  identity or connecting an existing one.
  """

  @type t() :: :commit | :decrypt | :commit_and_decrypt

  @spec commit() :: :commit
  def commit(), do: :commit

  @spec decrypt() :: :decrypt
  def decrypt(), do: :decrypt

  @spec commit_and_decrypt() :: :commit_and_decrypt
  def commit_and_decrypt(), do: :commit_and_decrypt
end
