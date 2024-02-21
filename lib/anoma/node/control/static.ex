defmodule Anoma.Node.Control.Static do
  @moduledoc """
  I implement the static configuration engine.

  The user can query to ask the value of the configuration key relative to the storage.
  """

  alias Anoma.Storage

  def get(storage, key) do
    {key, Storage.get(storage, [:static, key])}
  end
end
