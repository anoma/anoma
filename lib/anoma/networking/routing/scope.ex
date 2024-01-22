defmodule Anoma.Networking.Routing.Scope do
  @moduledoc """
  I am kind of overkill. I note if the given message should stay local
  or be forwarded to other nodes
  """

  @type t() :: :forward | :local

  @local :local
  @spec local() :: :local
  def local(), do: @local

  @forward :forward
  @spec forward() :: :forward
  def forward(), do: @forward
end
