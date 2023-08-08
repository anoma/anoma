defmodule Taiga.Nullifier do
  # Should we tag this at some point?
  alias Taiga.Base
  @type t() :: {Base.t()}

  @spec default() :: t()
  def default() do
    {Base.default()}
  end
end
