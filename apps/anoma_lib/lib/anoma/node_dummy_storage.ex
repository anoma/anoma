defmodule Anoma.Node.DummyStorage do
  @moduledoc """
  I am dummy storage during the defactor, to avoid undue stress on shielded rm.
  """

  def get(_, _) do
    {:ok, true}
  end
end
